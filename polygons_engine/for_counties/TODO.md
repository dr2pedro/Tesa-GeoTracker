# Polígonos dos bairros

ToDo

- [ ] Refatorar a função para micro-funções e trocar os loops para: applys; foreach; vectorise; ou Rcpp. 

Bonus

- [ ] Inserir um `TryCatch` para verificar se a API do IBGE está ativa; 

- [ ] Inserir um IF caso a API não esteja ativa e exista um `.docker_nominatim` aberto para fazer o search via `http GET` pelos parâmetros fornecidos pela `alias_list`. 

- [ ] Desativar os slices caso as buscas tenham vindo pelo nominatim.

- [ ] Fazer um uma função que armazene um array para o slice específico do nominatim. 