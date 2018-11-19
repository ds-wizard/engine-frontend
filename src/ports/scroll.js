module.exports = function (app) {
    app.ports.scrollToTop.subscribe(scrollToTop);


    function scrollToTop(elementId) {
        var element = document.getElementById(elementId);
        if (element) {
            element.scrollTop = 0;
        }
    }
};
