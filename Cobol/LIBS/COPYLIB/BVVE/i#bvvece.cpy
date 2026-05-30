      ******************************************************************
      *                                                                *
      *   AREA     : I#BVVECE                                          *
      *   OBJETIVO : TABELA DE CODIGOS DE RESPOSTA - CIELO             *
      *                                                                *
      ******************************************************************
       01  WRK-TABELA-RESPOSTA.
           03  WRK-TAB-RESPOSTA.
               05  FILLER                     PIC X(82) VALUE
                  '01-Campo de preenchimento obrigatorio não preenchido
      -           ' '.
               05  FILLER                     PIC X(82) VALUE
                  '02-Campo com conteúdo invalido'.
               05  FILLER                     PIC X(82) VALUE
                  '03-Domicílio bancário invalido'.
               05  FILLER                     PIC X(82) VALUE
                  '04-Estabelecimento comercial expurgado do cadastro de
      -           ' estabelecimentos            '.
               05  FILLER                     PIC X(82) VALUE
                  '05-Estabelecimento comercial fechado por motivo fraud
      -           'e'.
               05  FILLER                     PIC X(82) VALUE
                  '06-Estabelecimento eh filial de cadeia centralizada,
      -           ' p/ alteracao enviar matriz'.
               05  FILLER                     PIC X(82) VALUE
                  '07-Estabelecimento comercial nao financeiro'.
               05  FILLER                     PIC X(82) VALUE
                  '08-Estabelecimento nao possui esse tipo de domicilio
      -           'bancario'.
               05  FILLER                     PIC X(82) VALUE
                  '09-Estabelecimento comercial possui trava de domicíli
      -           'o'.
               05  FILLER                     PIC X(82) VALUE
                  '10-Tipo de Conta invalido para o Tipo de Pessoa'.
               05  FILLER                     PIC X(82) VALUE
                  '11-Domicílio do Estabelecimento possui cessao de dire
      -           'ito - nao pode ser alterado'.
               05  FILLER                     PIC X(82) VALUE
                  '12-Estabelecimento comercial com moeda dolar - domici
      -           'lio nao pode ser alterado'.
               05  FILLER                     PIC X(82) VALUE
                  '13-Estabelecimentos de mesma raiz de CNPJ/CPF devem p
      -           'ossuir mesmo banco domicilio'.
               05  FILLER                     PIC X(82) VALUE
                  '14-Estabelecimentos com raiz de CNPJ/CPF diferentes c
      -           'om mesmo domicílio bancário'.
               05  FILLER                     PIC X(82) VALUE
                  '15-Estabelecimento eh correspondente bancario - domic
      -           'ilio não pode ser alterado'.
               05  FILLER                     PIC X(82) VALUE
                  '16-Divergencia de regras domicilio Visa X Visa Vale'.
               05  FILLER                     PIC X(82) VALUE
                  '17-Divergencia de regras domicilio Visa Vale X Visa'.
               05  FILLER                     PIC X(82) VALUE
                  '18-Estabelecimento comercial não localizado e CNPJ/CP
      -           'F não localizado'.
               05  FILLER                     PIC X(82) VALUE
                  '19-Estab. nao localizado, existe(m) outras maquineta(
      -           's) com o mesmo CNPJ/CPF'.
               05  FILLER                     PIC X(82) VALUE
                  '20-EC individual de mesma raiz de CNPJ/CPF da matriz
      -           'com domicilio diferente'.
               05  FILLER                     PIC X(82) VALUE
                  '21-Existem outros estab. de mesma raiz de CNPJ/CPF fe
      -           'chados por motivo fraude'.
               05  FILLER                     PIC X(82) VALUE
                  '22-Domicilio bancario ja cadastrado para o estab., so
      -           'licitacao nao acatada'.
               05  FILLER                     PIC X(82) VALUE
                  '23-Banco nao autorizado para alteracao automatica de
      -           'domicilio bancario'.
               05  FILLER                     PIC X(82) VALUE
                  '24-Banco nao cadastrado'.
               05  FILLER                     PIC X(82) VALUE
                  '25-CNPJ/CPF não é válido para o estabelecimento infor
      -           'mado'.
               05  FILLER                     PIC X(82) VALUE
                  '26-Estab. nao possui produto habilitado para a bandei
      -           'ra - nao pode ser alterado'.
               05  FILLER                     PIC X(82) VALUE
                  '27-EC indiv. de mesma raiz CNPJ/CPF nao passou pelas
      -           'regras Visa X Visa Vale'.
               05  FILLER                     PIC X(82) VALUE
                  '28-Domicilio Visa, do ec individual, esta securitizad
      -           'o. Alteracao nao permitida'.
               05  FILLER                     PIC X(82) VALUE
                  '29-Erro no acesso ao programa BIOPCACD'.
               05  FILLER                     PIC X(82) VALUE
                  '30-Ramo de atividade nao permite alteracao de domicil
      -           'io Visa Vale'.
               05  FILLER                     PIC X(82) VALUE
                  '31-Erro no acesso ao programa ESOSR06C (cálculo de di
      -           'gito verificador)'.
               05  FILLER                     PIC X(82) VALUE
                  '32-Erro na leitura (select) da tabela TBAEECBC por nu
      -           'mero do ec'.
               05  FILLER                     PIC X(82) VALUE
                  '33-Erro na leitura (select) da tabela TBAREXPU'.
               05  FILLER                     PIC X(82) VALUE
                  '34-Erro na leitura (select-07) da tabela TBAEECBC por
      -           ' CNPJ/CPF.'.
               05  FILLER                     PIC X(82) VALUE
                  '35-Erro na leitura (select-05) da tabela TBAEECBC por
      -           ' numero do ec'.
               05  FILLER                     PIC X(82) VALUE
                  '36-Erro na leitura (read) do arquivo MMIDXKS'.
               05  FILLER                     PIC X(82) VALUE
                  '37-Erro na leitura (read) do arquivo MMMSTKS'.
               05  FILLER                     PIC X(82) VALUE
                  '38-Estabelecimento nao localizado e digito verificado
      -           'r de CNPJ/CPF invalido'.
               05  FILLER                     PIC X(82) VALUE
                  '39-Erro na leitura (select) da tabela TBESECBD por ec
      -           ' e tipo de domicilio'.
               05  FILLER                     PIC X(82) VALUE
                  '40-Erro no acesso a subrotina ESOPR07D'.
               05  FILLER                     PIC X(82) VALUE
                  '41-Erro na leitura das tabelas TBESPREC e TBESRLC (jo
      -           'in)'.
               05  FILLER                     PIC X(82) VALUE
                  '42-Erro na leitura (read) do arquivo MMPRMKS'.
               05  FILLER                     PIC X(82) VALUE
                  '43-Erro na leitura (startbr) do arquivo CD001KS'.
               05  FILLER                     PIC X(82) VALUE
                  '44-Erro na leitura (readnext) do arquivo CD001KS'.
               05  FILLER                     PIC X(82) VALUE
                  '45-Erro na leitura da tabela TBEPPEDI'.
               05  FILLER                     PIC X(82) VALUE
                  '46-Erro na leitura (read) do arquivo TABELAV'.
               05  FILLER                     PIC X(82) VALUE
                  '47-Erro na abertura (open) ou fechamento (close) do c
      -           'cursor CONTAS-DUPLICADAS'.
               05  FILLER                     PIC X(82) VALUE
                  '48-Erro na leitura (fetch) do cursor CONTAS-DUPLICADA
      -           'S'.
               05  FILLER                     PIC X(82) VALUE
                  '49-Erro na leitura (select-03) da tabela TBAEECBC por
      -           ' numero do ec'.
               05  FILLER                     PIC X(82) VALUE
                  '50-Erro na leitura (select) da tabela TBESECNT'.
               05  FILLER                     PIC X(82) VALUE
                  '51-Erro na leitura (read) do arquivo VVTABELA'.
               05  FILLER                     PIC X(82) VALUE
                  '52-Domicilio Visa securitizado. Alteracao nao permiti
      -           'da p/ tipo de domicilio 0'.
               05  FILLER                     PIC X(82) VALUE
                  '53-Codigo retorno no acesso a subrotina ESOPR07D dife
      -           'rente de zeros'.
               05  FILLER                     PIC X(82) VALUE
                  '54-Domicilio Visa Vale securitizado. Alteracao nao pe
      -           'rmitida p/ tipo domicilio 0'.
               05  FILLER                     PIC X(82) VALUE
                  '55-Domicilio securitizado. Alteracao de domicilio nao
      -           ' permitida'.
               05  FILLER                     PIC X(82) VALUE
                  '56-Erro no acesso a subrotina BIOPCACD'.
               05  FILLER                     PIC X(82) VALUE
                  '57-Codigo retorno no acesso a subrotina BIOPCACD dife
      -           'rente de zeros'.
               05  FILLER                     PIC X(82) VALUE
                  '58-Erro ao inserrir (insert) registro na tabela TBESA
      -           'LTD'.
               05  FILLER                     PIC X(82) VALUE
                  '59-Erro na leitura (select) da tabela TBESRLC'.
               05  FILLER                     PIC X(82) VALUE
                  '60-Erro no acesso a subrotina ATOS001B'.
               05  FILLER                     PIC X(82) VALUE
                  '61-Erro na abetura (open) do cursor RAIZ-CNPJ-ECBC-IN
      -           'D - tabela TBAEECBC'.
               05  FILLER                     PIC X(82) VALUE
                  '62-Erro na leitura (fetch) do cursor RAIZ-CNPJ-ECBC-I
      -           'ND - tabela TBAEECBC'.
               05  FILLER                     PIC X(82) VALUE
                  '63-Erro na abetura (open) do cursor RAIZ-CNPJ-ECBD-IN
      -           'D - tabela TBESECBD'.
               05  FILLER                     PIC X(82) VALUE
                  '64-Erro na leitura (fetch) do cursor RAIZ-CNPJ-ECBD-I
      -           'ND - tabela TBESECBD'.
               05  FILLER                     PIC X(82) VALUE
                  '65-Erro na abetura (open) do cursor MOVTO-ALTERA-EC -
      -           ' tabela TBAEECBC'.
               05  FILLER                     PIC X(82) VALUE
                  '66-Erro na leitura (fetch) do cursor MOVTO-ALTERA-EC
      -           '- tabela TBAEECBC'.
               05  FILLER                     PIC X(82) VALUE
                  '67-Erro no momento da atualizacao do domicilio - ec n
      -           'ao encontrado na TBAEECBC'.
               05  FILLER                     PIC X(82) VALUE
                  '68-Erro na leitura para atualização (read update) do
      -           'arquivo MMMSTKS'.
               05  FILLER                     PIC X(82) VALUE
                  '69-Erro na atualizacao (rewrite) do arquivo MMMSTKS'.
               05  FILLER                     PIC X(82) VALUE
                  '70-Erro na atualizacao (update 01) da tabela TBAEECBC
      -           ' '.
               05  FILLER                     PIC X(82) VALUE
                  '71-Acesso a subrotina ATOS001B com código de retorno
      -           'diferente de zeros'.
               05  FILLER                     PIC X(82) VALUE
                  '72-Erro no acesso a subrotina BCOS001B'.
               05  FILLER                     PIC X(82) VALUE
                  '73-Acesso a subrotina BCOS001B com código de retorno
      -           'diferente 00, 01, 02, 03'.
               05  FILLER                     PIC X(82) VALUE
                  '74-Erro na leitura (select 05) da tabela TBESECBD'.
               05  FILLER                     PIC X(82) VALUE
                  '75-Erro na leitura (select 04) da tabela TBESECBD'.
               05  FILLER                     PIC X(82) VALUE
                  '76-Erro na leitura para atualizacao (read update) do
      -           'arquivo MMPRMKS'.
               05  FILLER                     PIC X(82) VALUE
                  '77-Erro na atualizacao (rewrite) do arquivo MMPRMKS'.
               05  FILLER                     PIC X(82) VALUE
                  '78-Erro na atualizacao (update 02) da tabela TBAEECBC
      -           ' '.
               05  FILLER                     PIC X(82) VALUE
                  '79-Erro ao inserir (insert) registro na tabela TBESEC
      -           'BH'.
               05  FILLER                     PIC X(82) VALUE
                  '80-Erro ao deletar (delete) registro da tabela TBESEC
      -           'BD'.
               05  FILLER                     PIC X(82) VALUE
                  '81-Erro na atualização (update 01) da tabela TBESECBD
      -           ' '.
               05  FILLER                     PIC X(82) VALUE
                  '82-Erro na atualizacao (update 01) da tabela TBESECBD
      -           ' '.
               05  FILLER                     PIC X(82) VALUE
                  '83-Erro no acesso a subrotina ATOS005B'.
               05  FILLER                     PIC X(82) VALUE
                  '84-Acesso a subrotina ATOS005B com codigo de retorno
      -           'diferente de zeros'.
               05  FILLER                     PIC X(82) VALUE
                  '85-Erro na abertura (open) do cursor VERIF-RAIZ-CNPJ
      -           ' '.
               05  FILLER                     PIC X(82) VALUE
                  '86-Erro na leitura (fetch) do cursor VERIF-RAIZ-CNPJ
      -           ' '.
               05  FILLER                     PIC X(82) VALUE
                  '87-Erro no start do arquivo CD001KS'.
               05  FILLER                     PIC X(82) VALUE
                  '88-Erro no start do arquivo CD001KS'.
               05  FILLER                     PIC X(82) VALUE
                  '89-Erro ao inserir (insert) registro na TBAEECBH'.
               05  FILLER                     PIC X(82) VALUE
                  '90-Erro na leitura (select 04) da tabela TBAEECBC'.
               05  FILLER                     PIC X(82) VALUE
                  '91-Erro na leitura (select 02) da tabela TBESECBD'.
               05  FILLER                     PIC X(82) VALUE
                  '92-Erro ao deletar (delete) registro do arquivo MMNMS
      -           'KS'.
               05  FILLER                     PIC X(82) VALUE
                  '93-Erro ao ler para atualizar (read update) o arquivo
      -           ' MMNMSKS'.
               05  FILLER                     PIC X(82) VALUE
                  '94-Erro ao atualizar (rewrite) o arquivo MMNMSKS'.
               05  FILLER                     PIC X(82) VALUE
                  '95-Erro ao gravar (write) o arquivo MMNMSKS'.
               05  FILLER                     PIC X(82) VALUE
                  '96-Erro na abertura (open) do cursor VERIF-RAIZ-CNPJ-
      -           'ECBD'.
               05  FILLER                     PIC X(82) VALUE
                  '97-Erro na leitura (fetch) do cursor VERIF-RAIZ-CNPJ-
      -           'ECBD'.
               05  FILLER                     PIC X(82) VALUE
                  '98-Erro na atualização (update 02) da tabela TBESECBD
      -           ' '.
               05  FILLER                     PIC X(82) VALUE
                  '99-Erro geral'.
      *
           03  WRK-TAB-RESPOSTAS REDEFINES WRK-TAB-RESPOSTA OCCURS 99.
               05  WRK-TAB-DESC-ERROS.
                   07  WRK-TAB-COD-ERRO      PIC 9(02).
                   07  FILLER                PIC X(01).
                   07  WRK-TAB-MSG-ERRO      PIC X(79).
