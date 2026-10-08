# Собираю статическое демо: интерфейс и настоящее ядро F# в WebAssembly.
param([ValidatePattern('^/(?:[A-Za-z0-9._~-]+/)*$')][string]$BasePath = '/')
$ErrorActionPreference = 'Stop'
$publish = Join-Path $PSScriptRoot 'artifacts/site'
dotnet publish (Join-Path $PSScriptRoot 'lab/Browser/Browser.csproj') -c Release -o (Join-Path $PSScriptRoot 'artifacts/publish') --disable-build-servers -m:1
if ($LASTEXITCODE -ne 0) {throw 'Сборка демо завершилась ошибкой'}
New-Item -ItemType Directory -Path $publish -Force | Out-Null
Copy-Item -Path (Join-Path $PSScriptRoot 'artifacts/publish/wwwroot/*') -Destination $publish -Recurse -Force
$pagePath = Join-Path $publish 'index.html'
$html = [IO.File]::ReadAllText($pagePath).Replace('@BASE_PATH@', $BasePath)
foreach ($asset in @('app.js','runtime.js','styles.css')) {
  $hash=(Get-FileHash -LiteralPath (Join-Path $publish $asset)).Hash.Substring(0,12).ToLowerInvariant()
  $html=$html.Replace('"' + $asset + '"','"' + $asset + '?v=' + $hash + '"')
}
[IO.File]::WriteAllText($pagePath,$html,[Text.UTF8Encoding]::new($false))
[IO.File]::WriteAllText((Join-Path $publish '.nojekyll'),'')
Write-Output "Готовое демо: $publish"
