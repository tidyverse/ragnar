# /// script
# requires-python = ">=3.10"
# dependencies = [
#     "embedding-atlas",
#     "duckdb",
# ]
# ///

import embedding_atlas

from embedding_atlas.projection import compute_vector_projection
from embedding_atlas.data_source import DataSource
from embedding_atlas.server import make_server

import uvicorn
import pathlib
import threading


def run_embedding_atlas(df, host, port):
    df = df.to_pandas()

    compute_vector_projection(df, "embedding")
    df["_row_index"] = range(len(df))  # add a row index for neighbors
    df.drop(columns=["embedding"], inplace=True)

    metadata = {
        "columns": {
            "id": "_row_index",
            "text": "text",
            "embedding": {
                "x": "projection_x",
                "y": "projection_y",
            },
            "neighbors": "neightbors",
        },
    }

    dataset = DataSource("<identifier>", df, metadata)

    static = str((pathlib.Path(embedding_atlas.__file__).parent / "static").resolve())
    app = make_server(dataset, static_path=static, duckdb_uri="server")

    config = uvicorn.Config(app, host=host, port=port, access_log=False)
    server = uvicorn.Server(config)

    thread = threading.Thread(target=server.run)
    thread.start()

    def join_app_thread():
        try:
            thread.join()
        except KeyboardInterrupt:
            server.should_exit = True
            thread.join()

    return join_app_thread
