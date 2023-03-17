1. 执行Gau_vibr2xyz.exe，按提示输入Gaussian freq任务输出文件名，需要的模式编号和每个振动方向放多少帧，
2. VMD载入新生成的xyz文件，调好效果后执行source generate_transparent_povray_files.tcl
3. 安装好Pov-Ray 3.7，把安装目录下的bin目录加入环境变量PATH，执行render_povray.bat
4. 执行python png2gif_animate_transparent.py --loop 0 --duration 0.02，得到的out.gif就是动画
