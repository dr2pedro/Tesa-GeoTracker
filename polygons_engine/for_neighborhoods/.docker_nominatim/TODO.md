# Nominatim Docker Pedro's Fork (Nominatim version 3.5)

ToDo

- [X] Buildar a imagem já com o setup de algum osm.pbf feito e com o entrypoint no `start.sh` sem a necessidade de obrigatoriamente indexar um volume na linha de comando inserindo um `VOLUME` no `Dockerfile` da pasta source dos postgresdata;

- [X] Inserir os comentários no `Dockerfile` especificando quais processos estão sendo executados por comandos (alguns já vêm por padrão);

- [ ] Verificar se o postgresdata feito pelo `setup` na pasta `/data/postgresdata` foi sincronizado para o diretório default do Postgres `/var/lib/postgresql/12/main`;

- [X] Fazer na imagem um `--build-args` que insira em minísculo o nome da macroregião do Brasil, para se tornar multi-stage;

- [ ] Inserir em um repositório separado;

Bonus
    
- [ ] Fazer da imagem multi-arquitetura e configurar um github actions para autopush no docker hub. 

