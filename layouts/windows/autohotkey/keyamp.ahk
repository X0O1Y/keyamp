F1::
{
  Send "{LAlt Down}{Tab}"
  Send "{LAlt Up}"
}
return

^h::Send "{Left}"
^s::Send "{Right}"
^d::Send "{Up}"
^t::Send "{Down}"

^Space::Send "{Down}"
^BackSpace::Send "{Up}"

F11::
{
    if WinExist("ahk_exe emacs.exe")
        WinActivate
    else
        Run "C:\Users\em\emacs-29.1\bin\runemacs.exe"        
}

F2::
{
    if WinExist("ahk_exe chrome.exe")
        WinActivate
}

F3::
{
    if WinExist("ahk_exe dbeaver.exe")
        WinActivate
}

F4::
{
    if WinExist("ahk_exe Ssms.exe")
        WinActivate
}

