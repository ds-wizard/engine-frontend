var chartjs = require('chart.js')

chartjs.Chart.register(
    chartjs.RadarController,
    chartjs.RadialLinearScale,
    chartjs.PointElement,
    chartjs.LineElement,
    chartjs.Filler,
    chartjs.Tooltip,
)

module.exports = function (app) {
    app.ports.drawMetricsChart.subscribe(drawMetricsChart)

    function drawMetricsChart(data) {
        setTimeout(function () {
            var canvas = document.getElementById(data.targetId)
            if (!canvas) {
                return
            }

            var ctx = canvas.getContext('2d')
            var chart = new chartjs.Chart(ctx, {
                type: 'radar',
                data: data.data,
                options: {
                    aspectRatio: 2,
                    scales: {
                        r : {
                            min: 0,
                            max: 1,
                            ticks: {
                                stepSize: .25,
                                display: false,
                            }
                        },
                    }
                }
            })
        }, 100)
    }
}
