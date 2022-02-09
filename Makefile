deploy:
    git checkout prod && git pull && git merge master -m "chore: forward changes to prod" && git push origin && git checkout master