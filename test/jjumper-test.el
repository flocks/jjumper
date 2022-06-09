(ert-deftest extract-keys-from-json ()
  (let ((json '((name . "v1")
				(version . "v2")
				(scripts (vault ( sub . 1))
						 (dev . "v4"))) ))
	(should (string-equal
			 "name version scripts.vault.sub scripts.dev"
			 (jjumper--traverse-object json "")))))

(ert-deftest extract-keys-with-arrays-from-json ()
  (let ((json '((name . "v1")
				(version . "v2")
				(scripts 
				 (vault (sub . 1))
				 (dev . "v4"))
				(dependencies . ["1" "2"]) (packages . [((id . 1) (name . "name"))]))
			  ))
	(should (string-equal
			 "name version scripts.vault.sub scripts.dev dependencies packages[0].id packages[0].name"
			 (jjumper--traverse-object json "")))))
