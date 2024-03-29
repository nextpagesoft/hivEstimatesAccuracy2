import React from 'react';
import ReactEchartsCore from 'echarts-for-react/lib/core';
import * as echarts from 'echarts/core';
import { BarChart, HeatmapChart } from 'echarts/charts';
import {
  GridComponent,
  ToolboxComponent,
  TooltipComponent,
  LegendPlainComponent,
  VisualMapComponent,
  VisualMapPiecewiseComponent,
  DatasetComponent
} from 'echarts/components';
import { SVGRenderer } from 'echarts/renderers';
import FormatPercentage from '../../utilities/FormatPercentage';

echarts.use([
  GridComponent,
  ToolboxComponent,
  TooltipComponent,
  LegendPlainComponent,
  DatasetComponent,
  VisualMapComponent,
  VisualMapPiecewiseComponent,
  BarChart,
  HeatmapChart,
  SVGRenderer
]);

const MissChart = (props) => {

  const { xCategories, data1, data2, data3, options } = props;

  const defaultOptions = {
    textStyle: {
      fontFamily: '"Roboto", "Helvetica", "Arial", sans-serif'
    },
    grid: [
      {
        left: 5,
        width: '30%',
        height: '347px',
        containLabel: true,
        tooltip: {
          formatter: params => `${params.marker} ${params.name}: ${FormatPercentage(params.value, 2)}`
        }
      },
      {
        left: 'center',
        width: '30%',
        height: '347px',
        containLabel: true,
        tooltip: {
          formatter: params => `${params.marker} ${params.name}: ${params.value[2] ? 'Present' : 'Missing'}`
        }
      },
      {
        right: 15,
        width: '30%',
        height: '300px',
        containLabel: true,
        tooltip: {
          formatter: params => `${params.marker} Combination ${params.name}: ${FormatPercentage(params.value, 2)}`
        }
      }
    ],
    xAxis: [
      {
        gridIndex: 0,
        type: 'category',
        data: xCategories,
        axisLabel: {
          rotate: 45
        },
        axisTick: {
          show: false
        },
        axisLine: {
          show: false
        }
      },
      {
        gridIndex: 1,
        type: 'category',
        data: xCategories,
        splitArea: {
          show: true
        },
        axisLabel: {
          rotate: 45
        },
        axisTick: {
          show: false
        },
        axisLine: {
          show: false
        }
      },
      {
        gridIndex: 2,
        axisTick: {
          show: false
        },
        axisLine: {
          show: false
        },
        axisLabel: {
          formatter: (val) => FormatPercentage(val, 0)
        }
      },
    ],
    yAxis: [
      {
        id: 'test',
        gridIndex: 0,
        name: 'Relative frequency of missing data',
        nameTextStyle: {
          align: 'left'
        },
        nameLocation: 'end',
        axisTick: {
          show: false
        },
        axisLine: {
          show: false
        },
        axisLabel: {
          formatter: (val) => FormatPercentage(val, 0)
        }
      },
      {
        gridIndex: 1,
        type: 'category',
        name: 'Missing data pattern',
        nameTextStyle: {
          align: 'left'
        },
        nameLocation: 'end',
        axisLabel: {
          show: false
        },
        axisLine: {
          show: false
        },
        axisTick: {
          show: false
        }
      },
      {
        gridIndex: 2,
        type: 'category',
        axisTick: {
          show: false
        },
        axisLine: {
          show: false
        }
      },
      {
        gridIndex: 2,
        position: 'right',
        type: 'category',
        data: data3[2],
        axisTick: {
          show: false
        },
        axisLine: {
          show: false
        }
      },
    ],
    visualMap: {
      type: 'piecewise',
      seriesIndex: 1,
      min: 0,
      max: 1,
      pieces: [
        { min: 0, max: 0.9999999, color: '#ddd', label: 'Missing' },
        { min: 0.9999999, color: '#69b023', label: 'Present' }
      ],
      show: false
    },
    series: [
      {
        type: 'bar',
        barCategoryGap: 1,
        color: '#cccccc',
        data: data1,
        xAxisIndex: 0,
        yAxisIndex: 0,
        visualMap: false
      },
      {
        type: 'heatmap',
        data: data2,
        xAxisIndex: 1,
        yAxisIndex: 1,
        itemStyle: {
          borderColor: '#fff',
          borderWidth: 1
        },
        emphasis: {
          itemStyle: {
            borderColor: '#fff',
            borderWidth: 1
          }
        }
      },
      {
        name: 'Present',
        type: 'bar',
        data: data3[0],
        xAxisIndex: 2,
        yAxisIndex: 2,
        barCategoryGap: 1,
        color: '#69b023',
        stack: true,
      },
      {
        name: 'Missing',
        type: 'bar',
        data: data3[1],
        xAxisIndex: 2,
        yAxisIndex: 2,
        barCategoryGap: 1,
        color: '#ccc',
        stack: true,
      }
    ],
    tooltip: {
      trigger: 'item'
    },
    toolbox: {
      show: true,
      feature: {
        dataZoom: {
          yAxisIndex: 'none'
        },
        saveAsImage: {
          pixelRatio: 2,
          name: 'MissingnessPattern',
          title: 'Save'
        }
      }
    },
    legend: { }
  };

  const finalOptions = Object.assign({}, defaultOptions, options);

  return (
    <ReactEchartsCore
      echarts={echarts}
      option={finalOptions}
      style={{ height: '410px', width: '100%' }}
      notMerge={true}
      lazyUpdate={true}
      opts={{}}
    />
  );
};

export default MissChart;
