;;; macher-tests.el --- Tests for macher.el -*- lexical-binding: t -*-

;;; Commentary:
;; Unit tests for macher.el functionality.

;;; Code:

(require 'ert)
(require 'macher)

;; Unit tests for `macher-context-buffers-for-file'.
(ert-deftest macher-test-context-buffers-for-file ()
  "Test `macher-context-buffers-for-file' with various scenarios."
  ;; Create a test context.
  (let* ((context (macher--make-context))
         (temp-file (make-temp-file "macher-test"))
         buffers)
    (unwind-protect
        (progn
          ;; Test getting buffers for a file that exists on disk.
          (setq buffers (macher-context-buffers-for-file temp-file context))
          (should buffers)
          (should (car buffers)) ;; Original buffer should exist.
          (should (cdr buffers)) ;; New buffer should exist.
          (should (buffer-live-p (car buffers)))
          (should (buffer-live-p (cdr buffers)))

          ;; Test getting the same buffers again (should be cached).
          (let ((cached-buffers (macher-context-buffers-for-file temp-file context)))
            (should (eq (car buffers) (car cached-buffers)))
            (should (eq (cdr buffers) (cdr cached-buffers))))

          ;; Test getting buffers for a non-existent file.
          (let* ((non-existent (concat temp-file "-nonexistent"))
                 (non-existent-buffers (macher-context-buffers-for-file non-existent context)))
            (should non-existent-buffers)
            (should-not (car non-existent-buffers)) ;; Original buffer should be nil.
            (should-not (cdr non-existent-buffers)) ;; New buffer should be nil.

            ;; Should be added to context's buffers list.
            (should
             (assoc (macher--normalize-path non-existent) (macher-context-buffers context)))))

      ;; Cleanup.
      (when (file-exists-p temp-file)
        (delete-file temp-file))
      (when buffers
        (when (buffer-live-p (car buffers))
          (kill-buffer (car buffers)))
        (when (buffer-live-p (cdr buffers))
          (kill-buffer (cdr buffers)))))))

;; Unit tests for `macher-context-set-new-buffer-for-file'
(ert-deftest macher-test-context-set-new-buffer-for-file ()
  "Test `macher-context-set-new-buffer-for-file' functionality."
  ;; Create a test context and buffers.
  (let* ((context (macher--make-context))
         (temp-file (make-temp-file "macher-test"))
         (original-buffers)
         (test-buffer (generate-new-buffer "*macher-test-buffer*")))

    (unwind-protect
        (progn
          ;; First get the original buffers.
          (setq original-buffers (macher-context-buffers-for-file temp-file context))

          ;; Set a new buffer for the file.
          (macher-context-set-new-buffer-for-file temp-file test-buffer context)

          ;; Check that the new buffer was set correctly.
          (let ((updated-buffers (macher-context-buffers-for-file temp-file context)))
            (should (eq (car updated-buffers) (car original-buffers))) ;; Original buffer unchanged.
            (should (eq (cdr updated-buffers) test-buffer)) ;; New buffer updated.

            ;; Check the context's buffers alist was updated.
            (let ((entry
                   (assoc (macher--normalize-path temp-file) (macher-context-buffers context))))
              (should entry)
              (should (eq (cadr entry) (car original-buffers)))
              (should (eq (cddr entry) test-buffer))))

          ;; Test setting to nil (for file deletion).
          (macher-context-set-new-buffer-for-file temp-file nil context)
          (let ((deletion-buffers (macher-context-buffers-for-file temp-file context)))
            (should (eq (car deletion-buffers) (car original-buffers)))
            (should-not (cdr deletion-buffers))

            ;; Check the context's buffers alist was updated for deletion.
            (let ((entry
                   (assoc (macher--normalize-path temp-file) (macher-context-buffers context))))
              (should entry)
              (should (eq (cadr entry) (car original-buffers)))
              (should-not (cddr entry)))))

      ;; Cleanup.
      (when (file-exists-p temp-file)
        (delete-file temp-file))
      (when (and original-buffers (buffer-live-p (car original-buffers)))
        (kill-buffer (car original-buffers)))
      (when (buffer-live-p test-buffer)
        (kill-buffer test-buffer)))))

(ert-deftest macher-test-project-files ()
  "Test `macher--project-files' and its default implementation."
  ;; Create a temporary directory structure for testing.
  (let* ((temp-dir (make-temp-file "macher-test-project" t))
         (file1 (expand-file-name "file1.txt" temp-dir))
         (file2 (expand-file-name "file2.el" temp-dir))
         (subdir (expand-file-name "subdir" temp-dir))
         (file3 (expand-file-name "file3.md" subdir))
         ;; Simulate a valid project root.
         (project-vc-extra-root-markers '(".project"))
         (project-file (expand-file-name ".project" temp-dir)))
    (unwind-protect
        (progn
          ;; Create test files.
          (make-directory subdir)
          (write-region "test" nil file1)
          (write-region "test" nil file2)
          (write-region "test" nil file3)

          ;; Write to the project file to make it a real project.
          (write-region "" nil project-file)

          ;; Test with nil project root.
          (should-error (macher--project-files nil))

          ;; Test getting project files.
          (let ((files (macher--project-files temp-dir)))
            (should files)
            (should (listp files))
            ;; Should contain relative paths.
            (should (member "file1.txt" files))
            (should (member "file2.el" files))
            (should (member "subdir/file3.md" files))))

      ;; Cleanup.
      (delete-directory temp-dir t))))

(ert-deftest macher-test-workspace-string ()
  "Test `macher--workspace-string' functionality."
  ;; Create a temporary directory structure for testing.
  (let* ((temp-dir (make-temp-file "macher-test-workspace" t))
         (file1 (expand-file-name "file1.txt" temp-dir))
         (file2 (expand-file-name "file2.el" temp-dir))
         (subdir (expand-file-name "subdir" temp-dir))
         (file3 (expand-file-name "file3.md" subdir))
         ;; Simulate a valid project root.
         (project-vc-extra-root-markers '(".project"))
         (project-file (expand-file-name ".project" temp-dir)))
    (unwind-protect
        (progn
          ;; Create test files.
          (make-directory subdir)
          (write-region "test content" nil file1)
          (write-region ";;; Elisp file content" nil file2)
          (write-region "# Markdown content" nil file3)

          ;; Write to the project file to make it a real project.
          (write-region "" nil project-file)

          ;; Test with project workspace.
          (let* ((workspace (cons 'project temp-dir))
                 (context (macher--make-context :workspace workspace))
                 (contexts `((,file1) (,file2)))
                 (result (macher--workspace-string contexts context)))
            (should (stringp result))
            ;; Should contain workspace information.
            (should (string-match-p "WORKSPACE CONTEXT" result))
            ;; Should contain our test files with full relative paths.
            (should (string-match-p "file1.txt" result))
            (should (string-match-p "file2.el" result))
            (should (string-match-p "subdir/file3.md" result))
            ;; Should mark files in context with [*].
            (should (string-match-p "\\[\\*\\] file1\\.txt" result))
            (should (string-match-p "\\[\\*\\] file2\\.el" result))
            ;; file3 should not be marked since it's not in contexts.
            (should (string-match-p "    subdir/file3\\.md" result))
            ;; Should contain workspace description.
            (should (string-match-p "In-memory editing environment" result)))

          ;; Test with single-file workspace.
          (let* ((workspace (cons 'file file1))
                 (context (macher--make-context :workspace workspace))
                 (contexts `((,file1)))
                 (result (macher--workspace-string contexts context)))
            (should (stringp result))
            ;; Should contain workspace information.
            (should (string-match-p "WORKSPACE CONTEXT" result))
            ;; Should contain only the single file.
            (should (string-match-p "file1.txt" result))
            ;; Should not contain other files.
            (should-not (string-match-p "file2.el" result))
            (should-not (string-match-p "file3.md" result))))

      ;; Cleanup.
      (delete-directory temp-dir t))))


;;; Tests for workspace detection functions.

;; Test fixtures and helpers for workspace tests.
(defvar macher-test--temp-dir nil
  "Temporary directory for tests.")

(defvar macher-test--temp-file nil
  "Temporary file for tests.")

(defun macher-test--setup-workspace-fixtures ()
  "Set up test fixtures for workspace tests."
  (setq macher-test--temp-dir (make-temp-file "macher-test" t))
  (setq macher-test--temp-file (make-temp-file "macher-test-file" nil ".txt"))
  ;; Create a simple file structure.
  (with-temp-file (expand-file-name "test.txt" macher-test--temp-dir)
    (insert "test content")))

(defun macher-test--teardown-workspace-fixtures ()
  "Clean up test fixtures for workspace tests."
  (when (and macher-test--temp-dir (file-exists-p macher-test--temp-dir))
    (delete-directory macher-test--temp-dir t))
  (when (and macher-test--temp-file (file-exists-p macher-test--temp-file))
    (delete-file macher-test--temp-file))
  (setq
   macher-test--temp-dir nil
   macher-test--temp-file nil))

(defmacro macher-test--with-workspace-fixtures (&rest body)
  "Run BODY with workspace test fixtures set up and torn down."
  `(unwind-protect
       (progn
         (macher-test--setup-workspace-fixtures)
         ,@body)
     (macher-test--teardown-workspace-fixtures)))

(defmacro macher-test--with-temp-buffer (file-name &rest body)
  "Create a temporary buffer visiting FILE-NAME and execute BODY."
  `(with-temp-buffer
     (when ,file-name
       (setq buffer-file-name ,file-name)
       (setq default-directory (file-name-directory ,file-name)))
     ,@body))

;; Tests for macher--project-workspace.

(ert-deftest macher-test--project-workspace-with-project ()
  "Test `macher--project-workspace' returns project workspace when in a project."
  (macher-test--with-workspace-fixtures
   ;; Mock project.el to return a project.
   (cl-letf (((symbol-function 'project-current)
              (lambda (&optional _may-prompt _dir)
                ;; Return a mock project object.
                (list 'transient macher-test--temp-dir)))
             ((symbol-function 'project-root) (lambda (_project) macher-test--temp-dir)))
     (macher-test--with-temp-buffer macher-test--temp-file
                                    (setq default-directory macher-test--temp-dir)
                                    (let ((result (macher--project-workspace)))
                                      (should (consp result))
                                      (should (eq (car result) 'project))
                                      (should (string= (cdr result) macher-test--temp-dir)))))))

(ert-deftest macher-test--project-workspace-without-project ()
  "Test `macher--project-workspace' returns nil when not in a project."
  (macher-test--with-workspace-fixtures
   ;; Mock project.el to return nil (no project).
   (cl-letf (((symbol-function 'project-current) (lambda (&optional _may-prompt _dir) nil)))
     (macher-test--with-temp-buffer macher-test--temp-file
                                    (setq default-directory macher-test--temp-dir)
                                    (let ((result (macher--project-workspace)))
                                      (should (null result)))))))

;; Tests for macher--file-workspace.

(ert-deftest macher-test--file-workspace-with-file ()
  "Test `macher--file-workspace' returns file workspace when buffer visits a file."
  (macher-test--with-workspace-fixtures
   (macher-test--with-temp-buffer macher-test--temp-file
                                  (let ((result (macher--file-workspace)))
                                    (should (consp result))
                                    (should (eq (car result) 'file))
                                    (should (string= (cdr result) macher-test--temp-file))))))

(ert-deftest macher-test--file-workspace-without-file ()
  "Test `macher--file-workspace' returns nil when buffer doesn't visit a file."
  (with-temp-buffer
    ;; Don't set buffer-file-name.
    (let ((result (macher--file-workspace)))
      (should (null result)))))

;; Tests for macher-workspace.

(ert-deftest macher-test-workspace-with-buffer-local-workspace ()
  "Test `macher-workspace' returns buffer-local workspace when set."
  (macher-test--with-workspace-fixtures
   (macher-test--with-temp-buffer macher-test--temp-file
                                  (setq-local macher--workspace '(custom . "/custom/path"))
                                  (let ((result (macher-workspace)))
                                    (should (consp result))
                                    (should (eq (car result) 'custom))
                                    (should (string= (cdr result) "/custom/path"))))))

(ert-deftest macher-test-workspace-project-detection ()
  "Test `macher-workspace' detects project workspace via hook."
  (macher-test--with-workspace-fixtures
   ;; Mock project.el to return a project.
   (cl-letf (((symbol-function 'project-current)
              (lambda (&optional _may-prompt _dir) (list 'transient macher-test--temp-dir)))
             ((symbol-function 'project-root) (lambda (_project) macher-test--temp-dir)))
     (macher-test--with-temp-buffer macher-test--temp-file
                                    (setq default-directory macher-test--temp-dir)
                                    (let ((result (macher-workspace)))
                                      (should (consp result))
                                      (should (eq (car result) 'project))
                                      (should (string= (cdr result) macher-test--temp-dir)))))))

(ert-deftest macher-test-workspace-file-fallback ()
  "Test `macher-workspace' falls back to file workspace when no project found."
  (macher-test--with-workspace-fixtures
   ;; Mock project.el to return nil (no project).
   (cl-letf (((symbol-function 'project-current) (lambda (&optional _may-prompt _dir) nil)))
     (macher-test--with-temp-buffer macher-test--temp-file
                                    (setq default-directory macher-test--temp-dir)
                                    (let ((result (macher-workspace)))
                                      (should (consp result))
                                      (should (eq (car result) 'file))
                                      (should (string= (cdr result) macher-test--temp-file)))))))

(ert-deftest macher-test-workspace-no-workspace-error ()
  "Test `macher-workspace' signals error when no workspace can be determined."
  (with-temp-buffer
    ;; Don't set buffer-file-name and mock hook to return nil.
    (let ((macher-workspace-hook nil))
      (should-error (macher-workspace) :type 'error))))

(ert-deftest macher-test-workspace-custom-hook ()
  "Test `macher-workspace' respects custom workspace hook functions."
  (macher-test--with-workspace-fixtures
   ;; Define a proper function symbol to avoid closure issues
   (fset 'macher-test--custom-workspace-fn (lambda () '(custom . "/custom/workspace")))
   (unwind-protect
       (let ((macher-workspace-hook (list 'macher-test--custom-workspace-fn)))
         (macher-test--with-temp-buffer macher-test--temp-file
                                        (let ((result (macher-workspace)))
                                          (should (consp result))
                                          (should (eq (car result) 'custom))
                                          (should (string= (cdr result) "/custom/workspace")))))
     ;; Clean up the function
     (fmakunbound 'macher-test--custom-workspace-fn))))

(ert-deftest macher-test-workspace-hook-order ()
  "Test `macher-workspace' respects hook function order."
  (macher-test--with-workspace-fixtures
   ;; Define proper function symbols to avoid closure issues
   (fset 'macher-test--first-fn (lambda () '(first . "/first")))
   (fset 'macher-test--second-fn (lambda () '(second . "/second")))
   (unwind-protect
       (let ((macher-workspace-hook (list 'macher-test--first-fn 'macher-test--second-fn)))
         (macher-test--with-temp-buffer macher-test--temp-file
                                        (let ((result (macher-workspace)))
                                          ;; Should return result from first function.
                                          (should (consp result))
                                          (should (eq (car result) 'first))
                                          (should (string= (cdr result) "/first")))))
     ;; Clean up the functions
     (fmakunbound 'macher-test--first-fn)
     (fmakunbound 'macher-test--second-fn))))

(ert-deftest macher-test-workspace-hook-nil-fallback ()
  "Test `macher-workspace' continues to next hook function when one returns nil."
  (macher-test--with-workspace-fixtures
   ;; Define proper function symbols to avoid closure issues
   (fset 'macher-test--nil-fn (lambda () nil))
   (fset 'macher-test--success-fn (lambda () '(success . "/success")))
   (unwind-protect
       (let ((macher-workspace-hook (list 'macher-test--nil-fn 'macher-test--success-fn)))
         (macher-test--with-temp-buffer macher-test--temp-file
                                        (let ((result (macher-workspace)))
                                          ;; Should return result from second function.
                                          (should (consp result))
                                          (should (eq (car result) 'success))
                                          (should (string= (cdr result) "/success")))))
     ;; Clean up the functions
     (fmakunbound 'macher-test--nil-fn)
     (fmakunbound 'macher-test--success-fn))))

(ert-deftest macher-test-workspace-with-different-buffer ()
  "Test `macher-workspace' works with explicit buffer argument."
  (macher-test--with-workspace-fixtures
   ;; Mock project.el to return a project.
   (cl-letf (((symbol-function 'project-current)
              (lambda (&optional _may-prompt _dir) (list 'transient macher-test--temp-dir)))
             ((symbol-function 'project-root) (lambda (_project) macher-test--temp-dir)))
     (macher-test--with-temp-buffer macher-test--temp-file
                                    (setq default-directory macher-test--temp-dir)
                                    (let ((test-buffer (current-buffer)))
                                      (with-temp-buffer
                                        ;; From different buffer, check the test buffer's workspace.
                                        (let ((result (macher-workspace test-buffer)))
                                          (should (consp result))
                                          (should (eq (car result) 'project))
                                          (should
                                           (string= (cdr result) macher-test--temp-dir)))))))))

;; Integration tests for workspace detection

(ert-deftest macher-test-workspace-integration-project-to-file ()
  "Test workspace detection falls back from project to file correctly."
  (macher-test--with-workspace-fixtures
   ;; Test the actual default hook behavior.
   (let ((macher-workspace-hook '(macher--project-workspace macher--file-workspace)))
     ;; Mock project.el to return nil.
     (cl-letf (((symbol-function 'project-current) (lambda (&optional _may-prompt _dir) nil)))
       (macher-test--with-temp-buffer macher-test--temp-file
                                      (setq default-directory macher-test--temp-dir)
                                      (let ((result (macher-workspace)))
                                        (should (consp result))
                                        (should (eq (car result) 'file))
                                        (should (string= (cdr result) macher-test--temp-file))))))))

(ert-deftest macher-test-workspace-integration-project-precedence ()
  "Test workspace detection prefers project over file when both available."
  (macher-test--with-workspace-fixtures
   ;; Test the actual default hook behavior.
   (let ((macher-workspace-hook '(macher--project-workspace macher--file-workspace)))
     ;; Mock project.el to return a project.
     (cl-letf (((symbol-function 'project-current)
                (lambda (&optional _may-prompt _dir) (list 'transient macher-test--temp-dir)))
               ((symbol-function 'project-root) (lambda (_project) macher-test--temp-dir)))
       (macher-test--with-temp-buffer macher-test--temp-file
                                      (setq default-directory macher-test--temp-dir)
                                      (let ((result (macher-workspace)))
                                        (should (consp result))
                                        (should (eq (car result) 'project))
                                        (should (string= (cdr result) macher-test--temp-dir))))))))

(provide 'macher-unit-tests)

;;; macher-unit-tests.el ends here
