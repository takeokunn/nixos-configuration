function ghq_create
    # Create github repo
    set license (curl https://api.github.com/licenses | jq -r ".[].spdx_id" | peco)
    gh repo create (echo $argv) --public --license $license -y
    ghq get git@github.com:takeokunn/(echo $argv).git

    # Add README.md
    cd (ghq root)/github.com/takeokunn/(echo $argv)
    echo "# "(echo $argv) >> README.md
    git add README.md
    git commit -m "Add README.md"
    git branch -M main
    git push -u origin main
end
