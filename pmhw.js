//var today = new Date(),
    //day = 1000 * 60 * 60 * 24,
    // Utility functions
    dateFormat = Highcharts.dateFormat,
    defined = Highcharts.defined,
    isObject = Highcharts.isObject,
    reduce = Highcharts.reduce;

// Set to 00:00:00:000 today
//today.setUTCHours(0);
//today.setUTCMinutes(0);
//today.setUTCSeconds(0);
//today.setUTCMilliseconds(0);
//today = today.getTime();

// The Chart
Highcharts.ganttChart('project-management-gantt-chart', {
    series: [{
        name: 'Suicide Data Product Design & Engineering',
        data: [{
            name: 'Data Product Design & Engineering',
            id: 'data_product',
            owner: 'Jonas'
        }, {
            name: 'Data Analytics',
            id: 'data_analytics',
            parent: 'data_product',
            start: Date.UTC(2019,11,3),
            end: Date.UTC(2019,11,16),
            completed: {
                amount: 1
            },
            owner: 'Jonas'
        }, {
            name: 'Predictive Model selection',
            id: "app_predict_model",
            dependency: 'data_analytics',
            parent: 'data_product',
            start: Date.UTC(2019,11,17),
            end: Date.UTC(2019,12,3),
            owner: 'Jonas'
        },{
            name: 'Application User Interface Design',
            id: 'App_UI_Design',
            dependency: 'app_predict_model',
            parent: 'data_product',
            start: Date.UTC(2019,12,3),
            end: Date.UTC(2019,12,23),
            owner: 'Jonas'
        }, {
            name: '1st Meeting with client',
            id: '1st_meeting',
            dependency: 'App_UI_Design',
            parent: 'data_product',
            start: Date.UTC(2019,12,24),
            milestone: true,
            owner: 'Jonas'
        }, {
            name: 'Data Product Creation',
            id: 'app_creation',
            dependency: '1st_meeting',
            parent: 'data_product',
            owner: 'Jonas'
        }, {
            name: 'Data Visulation Panel',
            id: 'data_vis',
            parent: 'app_creation',
            start: Date.UTC(2019,12,25),
            end: Date.UTC(2019,12,28),
            owner: 'Jonas'
        }, {
            name: 'Prediction and Text Cloud Panel',
            id: "data_pred",
            dependency: 'data_vis',
            parent: 'app_creation',
            start: Date.UTC(2019,12,28),
            end: Date.UTC(2019,12,31),
            owner: 'Jonas'
        }, {
            name: 'Data Table Panel',
            dependency: 'data_pred',
            parent: 'app_creation',
            start: Date.UTC(2019,12,31),
            end: Date.UTC(2020,1,3),
            owner: 'Jonas'
        }]
    }, {
        name: 'Product Evaluation and adjustment',
        data: [{
            name: 'Product Evaluation',
            id: 'product_evaluation',
            owner: 'Jonas'
        }, {
            name: 'Product Testing',
            id: 'testing',
            dependency: 'product_evaluation',
            parent: 'product_evaluation',
            start: Date.UTC(2020,1,4),
            end: Date.UTC(2020,1,6),
            owner: 'Jonas'
        },{
            name: '2nd Meeting with client',
            id: '2nd_meeting',
            dependency: 'testing',
            parent: 'product_evaluation',
            start: Date.UTC(2020,1,7),
            milestone: true,
            owner: 'Jonas'
        }, {
            name: 'Final adjustment and development',
            id: 'product_adjustment',
            dependency: '2nd_meeting',
            parent: 'product_evaluation',
            start: Date.UTC(2020,1,8),
            end: Date.UTC(2020,1,16),
            owner: 'Jonas'
        }, {
            name: 'Launch',
            dependency: 'product_adjustment',
            parent: 'product_evaluation',
            start: Date.UTC(2020,1,17),
            milestone: true,
            owner: 'Client'
        }]
    }],
    tooltip: {
        pointFormatter: function () {
            var point = this,
                format = '%e. %b',
                options = point.options,
                completed = options.completed,
                amount = isObject(completed) ? completed.amount : completed,
                status = ((amount || 0) * 100) + '%',
                lines;

            lines = [{
                value: point.name,
                style: 'font-weight: bold;'
            }, {
                title: 'Start',
                value: dateFormat(format, point.start)
            }, {
                visible: !options.milestone,
                title: 'End',
                value: dateFormat(format, point.end)
            }, {
                title: 'Completed',
                value: status
            }, {
                title: 'Owner',
                value: options.owner || 'unassigned'
            }];

            return reduce(lines, function (str, line) {
                var s = '',
                    style = (
                        defined(line.style) ? line.style : 'font-size: 0.8em;'
                    );
                if (line.visible !== false) {
                    s = (
                        '<span style="' + style + '">' +
                        (defined(line.title) ? line.title + ': ' : '') +
                        (defined(line.value) ? line.value : '') +
                        '</span><br/>'
                    );
                }
                return str + s;
            }, '');
        }
    },
    title: {
        text: 'Suicidal Data Product Project Management'
    },
    
    xAxis: {
        currentDateIndicator: true,
        //min: today - 3 * day,
        //max: today + 18 * day
    }
});