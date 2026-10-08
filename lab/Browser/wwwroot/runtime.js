"use strict";
let managedResolve;
const managedReady = new Promise(resolve => { managedResolve = resolve; });
window.labManagedStarted = () => managedResolve();
window.labReady = (async () => {
  await new Promise((resolve, reject) => {
    const script = document.createElement("script");
    script.src = "_framework/blazor.webassembly.js";
    script.setAttribute("autostart", "false");
    script.onload = resolve;
    script.onerror = () => reject(new Error("Не удалось загрузить вычислительное ядро. Проверьте подключение и обновите страницу."));
    document.head.append(script);
  });
  await window.Blazor.start();
  await managedReady;
  const result = JSON.parse(await window.DotNet.invokeMethodAsync("Portfolio.Browser", "Run", "health", "{}"));
  if (result.engine !== "F#") throw new Error("Проверка вычислительного ядра не пройдена.");
  return result;
})();
window.labReady.catch(() => {});
let previous = Promise.resolve();
window.labRun = (action, input) => {
  const next = previous.then(async () => {
    await window.labReady;
    await new Promise(resolve => setTimeout(resolve, 0));
    const result = JSON.parse(await window.DotNet.invokeMethodAsync("Portfolio.Browser", "Run", action, JSON.stringify(input)));
    if (result.error) throw new Error(result.error);
    return result;
  });
  previous = next.catch(() => {});
  return next;
};
