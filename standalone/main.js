const { shell, app, BrowserWindow } = require('electron');
const path = require('path');

function createWindow() {
    const win = new BrowserWindow({
        width: 800,
        height: 600,
        //TODO: update app to use customized icon. This is not functional across platform
        icon: path.join(__dirname, 'care_icon_small.png'),
        webPreferences: {
            preload: path.join(__dirname, 'preload.js') // optional, for security
        }
    });
    
    win.webContents.setWindowOpenHandler(({ url }) => {
        shell.openExternal(url);
        return { action: 'deny' };
    });
    
    win.maximize();
    win.loadFile('index.html'); // this is the file published from shiny app
}

app.whenReady().then(createWindow);

app.on('window-all-closed', () => {
    if (process.platform !== 'darwin') {
        app.quit();
    }
});



app.on('activate', () => {
    if (BrowserWindow.getAllWindows().length === 0) {
        createWindow();
    }
});
