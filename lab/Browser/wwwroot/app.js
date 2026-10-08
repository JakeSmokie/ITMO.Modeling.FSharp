"use strict";
const $ = id => document.getElementById(id);
let current = null, saved = null, ready = false, busy = false;
const nf = new Intl.NumberFormat("ru-RU",{maximumFractionDigits:1});
const num = n => nf.format(n);
const config = () => ({
  arrivalPerHour:Number($("arrival").value), serviceMinutes:Number($("service").value),
  intakeWorkers:Number($("intake").value), branchWorkers:Number($("branch").value),
  queueCapacity:Number($("capacity").value), branchProbability:Number($("probability").value),
  durationHours:Number($("hours").value), distribution:$("distribution").value,
  seed:Number($("seed").value), replications:Number($("replicas").value)
});
function controls(on) {
  busy = on;
  document.querySelectorAll("#config input,#config select").forEach(e=>e.disabled=on);
  $("calculate").disabled = on || !ready;
  $("calculate").textContent = on ? "Рассчитываю…" : "Рассчитать";
  document.querySelectorAll("[data-preset]").forEach(b => b.disabled = on);
  $("remember").disabled = on || !current;
  $("export").disabled = on || !current;
}
function drawChart(points) {
  const max = Math.max(1,...points.flatMap(p => [p.intake,p.branchA,p.branchB]));
  const last = points[points.length-1].hour || 1;
  const x = t => 44 + t/last*650, y = v => 190-v/max*160;
  let svg = '<line x1="44" y1="190" x2="694" y2="190"/><line x1="44" y1="30" x2="694" y2="30"/>';
  svg += '<text x="8" y="194">0</text><text x="8" y="34">'+num(max)+'</text><text x="44" y="215">0 ч</text><text x="654" y="215">'+num(last)+' ч</text>';
  [["intake","#14695f"],["branchA","#2364b3"],["branchB","#a35b15"]].forEach(([key,color]) => {
    const d=points.map((p,i)=>(i ? "L":"M")+x(p.hour).toFixed(2)+","+y(p[key]).toFixed(2)).join(" ");
    svg += '<path d="'+d+'" fill="none" stroke="'+color+'" stroke-width="2.5"/>';
  });
  $("chart").innerHTML=svg;
}
function describe(c) {
  return num(c.arrivalPerHour)+" заявок/ч · "+c.intakeWorkers+" на приёме · "+c.branchWorkers+" в каждой ветви";
}
function compare() {
  if (!saved || !current) return;
  $("comparison").hidden=false;
  $("comparison-note").textContent="Запомнено: "+describe(saved.config)+". Сейчас: "+describe(current.config)+".";
  const rows=[
    ["Завершено",saved.completed,current.completed],
    ["Потери, %",saved.lossPercent,current.lossPercent],
    ["Время в системе, мин",saved.meanTime,current.meanTime],
    ["Осталось в работе",saved.pending,current.pending]
  ];
  $("comparison").innerHTML='<table><thead><tr><th>Показатель</th><th>Запомненный</th><th>Текущий</th><th>Разница</th></tr></thead><tbody>'+
    rows.map(([label,a,b])=>'<tr><td>'+label+'</td><td>'+num(a)+'</td><td>'+num(b)+'</td><td>'+(b-a>0?"+":"")+num(b-a)+'</td></tr>').join("")+'</tbody></table>';
}
function render(r) {
  $("completed").textContent=num(r.completed);
  $("range").textContent="Разброс: "+r.completedMin+"–"+r.completedMax;
  $("time").textContent=num(r.meanTime)+" мин";
  $("loss").textContent=num(r.lossPercent)+" %";
  $("pending").textContent=num(r.pending);
  const names=["1. Приём","2A. Ветвь A","2B. Ветвь B"];
  $("flow").innerHTML=r.nodes.map((n,i)=>(i===1?'<span class="arrow" aria-hidden="true">→</span>':"")+
    '<div class="station"><h3>'+names[i]+'</h3><strong>'+num(n.utilization*100)+' %</strong><p>загрузка исполнителей</p><div class="meter"><i style="width:'+Math.min(100,n.utilization*100)+'%"></i></div><p>Ожидание: '+num(n.meanWait)+' мин</p><p>Средняя очередь: '+num(n.averageQueue)+'</p></div>').join("");
  $("balance").textContent="Баланс средних: "+num(r.arrived)+" поступило = "+num(r.completed)+" завершено + "+num(r.lost)+" потеряно + "+num(r.pending)+" в работе. "+r.config.replications+" прогонов. Числа округлены.";
  $("chart-note").textContent="Динамика первого прогона, seed "+r.config.seed+". Карточки показывают среднее по "+r.config.replications+" прогонам. Разброс — минимум и максимум, не доверительный интервал.";
  drawChart(r.sample.timeline);
  compare();
}
async function run() {
  if (!ready || busy || !$("config").reportValidity()) return;
  const input=config();
  controls(true); $("error").hidden=true;
  $("status").textContent="Вычисление "+input.replications+" прогонов…";
  try {
    current=await window.labRun("simulate",input);
    render(current);
    $("status").textContent="Расчёт готов. "+input.replications+" прогонов, seed "+input.seed+".";
    document.body.dataset.state="ready";
  } catch(e) {
    $("error").textContent=e.message; $("error").hidden=false;
    $("status").textContent="Проверьте параметры и повторите расчёт.";
    document.body.dataset.state="error";
  } finally { controls(false); }
}
$("config").addEventListener("submit",e=>{e.preventDefault();run();});
$("config").addEventListener("input",()=>{
  if (current && !busy) {
    $("status").textContent="Параметры изменены. Результаты относятся к предыдущему расчёту.";
    $("remember").disabled=true;
  }
});
document.querySelectorAll("[data-preset]").forEach(b=>b.addEventListener("click",()=>{
  const c={calm:[20,2,1],busy:[45,4,1],team:[45,4,2]}[b.dataset.preset];
  $("arrival").value=c[0]; $("intake").value=c[1]; $("branch").value=c[2];
  $("service").value=4; $("capacity").value=6; $("probability").value=0.5;
  $("hours").value=8; $("seed").value=42; $("replicas").value=12; $("distribution").value="exponential";
  if (ready) run();
}));
$("remember").addEventListener("click",()=>{
  if (!current) return;
  saved=structuredClone(current);
  $("comparison-note").textContent="Сценарий запомнен: "+describe(saved.config)+". Измените параметры и выполните расчёт.";
  $("comparison").hidden=true;
});
$("export").addEventListener("click",()=>{
  if (!current) return;
  const blob=new Blob([JSON.stringify({model:"finite-horizon-queue-network",engine:"F# .NET 10",current,comparison:saved},null,2)],{type:"application/json"});
  const url=URL.createObjectURL(blob), a=document.createElement("a");
  a.href=url; a.download="fsharp-queue-experiment.json"; a.click();
  setTimeout(()=>URL.revokeObjectURL(url),1000);
});
window.labReady.then(()=>{
  ready=true; $("engine").textContent="F# · вычисления в браузере"; controls(false); run();
}).catch(e=>{
  $("engine").textContent="Не удалось загрузить ядро";
  $("error").textContent=e.message; $("error").hidden=false;
  $("status").textContent="Обновите страницу, чтобы повторить загрузку.";
  document.body.dataset.state="error";
});
