module.exports = {
    createNavigatorData
}

function createNavigatorData() {
    return {
        pdf: getPdfSupport(),
        isMac: isMac()
    }
}

function getPdfSupport() {
    function hasAcrobatInstalled() {
        function getActiveXObject(name) {
            try {
                return new ActiveXObject(name)
            } catch (e) {
            }
        }

        return getActiveXObject('AcroPDF.PDF') || getActiveXObject('PDF.PdfCtrl')
    }

    function isIos() {
        return /iPad|iPhone|iPod/.test(navigator.userAgent) && !window.MSStream
    }

    return !!(navigator.mimeTypes['application/pdf'] || hasAcrobatInstalled() || isIos())
}

function isMac() {
    return /Macintosh|MacIntel|MacPPC|Mac68K/.test(navigator.userAgent)
}
