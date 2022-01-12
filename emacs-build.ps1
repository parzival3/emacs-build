$emacs_url = "https://ftp.gnu.org/gnu/emacs/windows/emacs-27/emacs-27.2-x86_64-no-deps.zip"
$emacs_zip = "emacs.zip"
Invoke-WebRequest -Uri $emacs_url -OutFile $emacs_zip
Expand-Archive -Path $emacs_zip 