var Chart = require('chart.js')


module.exports = function (app) {
    app.ports.drawMetricsChart.subscribe(drawMetricsChart)

    function drawMetricsChart(data) {
        setTimeout(function () {
            var canvas = document.getElementById(data.targetId)
            if (!canvas) {
                return
            }

            var ctx = canvas.getContext('2d')
            var chart = new Chart(ctx, {
                type: 'radar',
                data: data.data,
                options: {
                    legend: { display: false },
                    scale: {
                        ticks: {
                            min: 0,
                            max: 1,
                            maxTicksLimit: 5,
                            display: false
                        }
                    }
                }
            })
        }, 100)
    }
}
