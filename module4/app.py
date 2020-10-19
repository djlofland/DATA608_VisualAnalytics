import pandas as pd
import dash
import dash_table
import dash_core_components as dcc
import dash_html_components as html

import plotly.graph_objects as go
import plotly.figure_factory as ff


def encode_spaces(text):
    return text.replace(' ', '%20')


def df_to_plotly(df):
    return df.values.tolist(), df.columns.tolist(), df.index.tolist()


# === Define Constants ===
external_stylesheets = ['https://codepen.io/chriddyp/pen/bWLwgP.css']
HEALTH = ['Poor', 'Fair', 'Good']
STEWARD = ['None', '1or2', '3or4', '4orMore']

# === Load Borough Dataset ===
url = f"https://data.cityofnewyork.us/resource/nwxe-4ae8.json?"
select = f"$select=boroname,health,count(tree_id)"
where = f""
group = "&$group=boroname,health"

soql_url = url + encode_spaces(f'{select}{where}{group}&')
soql_trees = pd.read_json(soql_url)

borough_df = soql_trees.pivot(index='boroname', columns='health', values='count_tree_id')
borough_df = borough_df[HEALTH]

borough_ptn_df = borough_df.copy()
borough_ptn_df = round(100 * borough_ptn_df.div(borough_ptn_df.sum(axis=1), axis=0), 1)

# === Build Borough Heatmaps ===
z, x, y = df_to_plotly(borough_ptn_df)
borough_ptn_fig = ff.create_annotated_heatmap(x=x, y=y, z=z, colorscale='Greens')
borough_ptn_fig.update_layout(title='Tree Health by Borough (Percent)',
                              xaxis_title='Condition',
                              yaxis_title='Borough')

z, x, y = df_to_plotly(borough_df)
borough_fig = ff.create_annotated_heatmap(x=x, y=y, z=z, colorscale='Greens')
borough_fig.update_layout(title='Tree Health by Borough (Count)',
                          xaxis_title='Condition',
                          yaxis_title='Borough')

# === Load Steward Dataset ===
url = f"https://data.cityofnewyork.us/resource/nwxe-4ae8.json?"
select = f"$select=health,steward,count(tree_id)"
where = f""
group = "&$group=health,steward"

soql_url = url + encode_spaces(f'{select}{where}{group}&')
steward_trees = pd.read_json(soql_url).dropna()

steward_df = steward_trees.pivot(index='health', columns='steward', values='count_tree_id')
steward_df = steward_df[STEWARD]
steward_table = steward_df.reset_index()

steward_ptn_df = steward_df.copy()
steward_ptn_df = round(100 * steward_ptn_df.div(steward_ptn_df.sum(axis=0), axis=1), 2)
steward_ptn_table = steward_ptn_df.reset_index()

# === Build Steward Line plot ===
steward_fig = go.Figure()

colors = ['rgba(86, 112, 244, 1)', 'rgba(0, 205, 154, 1)', 'rgba(244, 79, 63, 1)']  # '#5670F4', '#00CD9A', '#F44F3F'
c_i = 0

x = steward_df.columns.tolist()

for row in steward_df.itertuples(index=True):
    label = row[0]
    y = row[1:]
    
    steward_fig.add_trace(go.Scatter(x=x,
                                     y=y,
                                     mode='lines+markers',
                                     name=label,
                                     line_color=colors[c_i])
                          )
    
    c_i += 1

steward_fig.update_layout(title='Tree Health by Steward',
                          xaxis_title='Steward',
                          yaxis_title='Count')

steward_fig.update_yaxes(type="log")

# === Setup Dash Dashboard Layout ===
app = dash.Dash(__name__, external_stylesheets=external_stylesheets)
server = app.server

app.layout = html.Div([
    html.H2('New York City Tree Census Dashboard'),
    html.H3('Donny Lofland, 10/13/2020'),
    html.H3('DS608 Module 4 (Dash)'),
    html.Hr(),
    html.Div([
        html.P('This data is collected by volunteers across the city, and is meant to catalog information about every single tree in the city.'),
        html.P('Build a dash app for a arborist studying the health of various tree species (as defined by the variable ‘spc_common’) across each borough (defined by the variable ‘borough’). This '
               'arborist would like to answer the following two questions for each species and in each borough:'),
    ]),
    html.Hr(),
    html.H4(html.I('What proportion of trees are in good, fair, or poor health according to the ‘health’ variable?')),
    html.Div([
        html.Div([
            dcc.Graph(figure=borough_ptn_fig),
        ], className="six columns"),
        html.Div([
            dcc.Graph(figure=borough_fig)
        ], className="six columns"),
    ], className="row"),
    html.Hr(),
    html.H4(html.I('Are stewards (steward activity measured by the ‘steward’ variable) having an impact on the health of trees?')),
    html.Div([
        html.Div([
            html.H5('Count of Trees by Health and Steward'),
            dash_table.DataTable(
                id='table',
                columns=[{"name": i, "id": i} for i in steward_table.columns],
                data=steward_table.to_dict('records'),
            ),
            html.H5('Portion of Trees by Health and Steward'),
            dash_table.DataTable(
                id='table2',
                columns=[{"name": i, "id": i} for i in steward_ptn_table.columns],
                data=steward_ptn_table.to_dict('records'),
            ),
        ], className="six columns"),
        html.Div([
            dcc.Graph(figure=steward_fig)
        ], className="six columns"),
        html.P("From this data, we see that the relative numbers of Poor, Fair, Good trees don't change based on the number of steward indicators.  If stewards had a correlation, "
               "we'd expect the percentages within a steward level to change, favoring Good.  Maybe there is a slight drop for the 'Poor' trees, but it's not a clear linear pattern and the slight "
               "effect could just be random noise."),
    ], className="row"),
])

if __name__ == '__main__':
    app.run_server(debug=True, host='127.0.0.1', processes=1)
