@echo off

if not NULL%1==NULL (
    pvengine64 %1.pov +UA +A +W1500 +H1500 +O%1.png /EXIT
) else (
    for %%i in (*.pov) do (
        pvengine64 %%i +UA +A +W1500 +H1500 +O%%~ni.png /EXIT
    )
)

