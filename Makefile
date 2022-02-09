.PHONY: deploy

deploy:
	git checkout prod
	git pull origin prod
	git merge master -m "chore: forward changes to prod"
	git push origin prod
	git checkout master