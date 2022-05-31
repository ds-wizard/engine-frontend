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
                    aspectRatio: 2,
                    plugins: {
                        legend: {
                            display: false
                        },
                    },
                    scales: {
                        r : {
                            min: 0,
                            max: 1,
                            ticks: {
                                stepSize: .25,
                                display: false
                            }
                        },
                    }
                }
            })
        }, 100)
    }
}
