$current_dir = $pwd
cd $env:USERPROFILE/Desktop
$emacs_url = "https://ftp.gnu.org/gnu/emacs/windows/emacs-27/emacs-27.2-x86_64-no-deps.zip"
$emacs_zip = "emacs.zip"
#Invoke-WebRequest -Uri $emacs_url -OutFile $emacs_zip
#Expand-Archive -Path $emacs_zip
.\emacs\bin\emacs.exe -Q --batch -l $current_dir\elisp-build\env.el -l $current_dir\elisp-build\eb.el --eval '(eb-perform-installation)'
