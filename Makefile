.PHONY: deploy

deploy:
	git checkout prod
	git pull origin master
	git merge master -m "chore: forward changes to prod"
	git push origin
	git checkout master