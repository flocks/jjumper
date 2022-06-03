(ert-deftest extract-keys-from-json ()
  (let ((json '((name . "v1")
				(version . "v2")
				(scripts (vault ( sub . 1))
						 (dev . "v4"))) ))
	(should (string-equal
			 "name version scripts.vault.sub scripts.dev"
			 (jjumper--get-keys json "")))))
