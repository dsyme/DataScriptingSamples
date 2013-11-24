rmdir /s /q packages
.nuget\NuGet.exe install  -ExcludeVersion -OutputDirectory packages
xcopy /y /q packages\R.NET\lib\net40\*  packages\RProvider\lib
xcopy /y /q packages\RDotNet.FSharp\lib\net40\*  packages\RProvider\lib
xcopy /y /q packages\Deedle.RPlugin\lib\net40\*  packages\RProvider\lib
