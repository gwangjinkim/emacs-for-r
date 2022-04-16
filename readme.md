Settings for r programming in emacs. (Still under construction).

place this folder into your home `~/emacs` in any Unix.
Usually, I create a (mini)conda environment for R. (If you already installed
anaconda, then you already have conda - so no need to do these steps):

```
# in ubuntu do
wget https://repo.anaconda.com/miniconda/Miniconda3-latest-Linux-x86_64.sh
bash Miniconda3-latest-Linux-x86_64.sh 
# follow instructions of the script


# in mac os X do:
brew install --cask miniconda
# or do:
wget https://repo.anaconda.com/miniconda/Miniconda3-latest-MacOSX-x86_64.sh
bash Miniconda3-latest-MacOSX-x86_64.sh
```

Let conda activate by default when starting bash shell (choose yes to that option!)

After that, create a conda environment for R
```
conda create --name r
conda activate r
conda install -c r r-base r-essentials
```


Add to your `~/.bashrc`:

```
alias remacs="conda activate r && env ORIG_HOME=$HOME HOME=$HOME/emacs/emacs-for-r emacs &"
```

Do `source ~/.bashrc`

In MacOSX if you already have a `.bash_profile` file, then this and not
`~/.bashrc` is run when starting a new shell. So to make '~/.bashrc` run,
add the line `source /path/to/your/.bashrc` into your `.bash_profile`.
Or, add the alias definition directly into your `.bash_profile`.


For Windows, I would follow the strategy, to install `scoop`, and use `scoop install`
to install `miniconda3`.

I will post PowerShell commands here which will be equivalent.
