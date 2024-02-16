use actix_web::{web, App, HttpResponse, HttpServer, Responder};

async fn greet() -> impl Responder {
    let curr_dir = std::env::current_dir().unwrap();
    let index_file_path = curr_dir.join("src/templates/index.html");
    let index_file_content = std::fs::read_to_string(index_file_path).unwrap();
    HttpResponse::Ok()
        .content_type("text/html")
        .body(index_file_content)
}

#[actix_web::main]
async fn main() -> std::io::Result<()> {
    HttpServer::new(|| {
        App::new()
            .service(actix_files::Files::new("/pkg", "./pkg"))
            .route("/", web::get().to(greet))
    })
    .bind(("127.0.0.1", 8082))?
    .run()
    .await
}
