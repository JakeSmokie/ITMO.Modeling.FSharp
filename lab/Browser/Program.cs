using Microsoft.AspNetCore.Components.WebAssembly.Hosting;
using Microsoft.JSInterop;
using System.Text.Json;
var host = WebAssemblyHostBuilder.CreateDefault(args).Build();
await host.Services.GetRequiredService<IJSRuntime>().InvokeVoidAsync("labManagedStarted");
await host.RunAsync();
public static class BrowserApi {
    [JSInvokable("Run")]
    public static string Run(string action, string input) {
        try {
            if (input.Length > 16384) throw new ArgumentException("Слишком большой запрос.");
            return Portfolio.Api.Run(action, input);
        } catch (Exception e) when (e is ArgumentException or InvalidOperationException or JsonException or FormatException or OverflowException) {
            return JsonSerializer.Serialize(new { error = e.Message });
        }
    }
}
