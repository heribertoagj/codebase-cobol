      *----------------------------------------------------------------*
      * ELEMENTO : BVVE - CARTAO DE CREDITO - VENDEDOR                 *
      *----------------------------------------------------------------*
      * BOOK     : I#BVVESL                                            *
      * DESCRICAO: BOOK DO ARQUIVO DE SOLICITACAO DE PARAMETRIZACAO DE *
      *            LIMITE TECNICO                                      *
      * EMPRESA  : FOURSYS                                             *
      * AUTOR    : FABRICA                                             *
      * TAMANHO  : 250  BYTES                                          *
      *----------------------------------------------------------------*
      * CAMPO                        | DESCRICAO                       *
      *----------------------------------------------------------------*
      * SPARM-LIM-CH-AGENCIA         | CHAVE AGENCIA                   *
      * SPARM-LIM-CH-CNPJ-CONTR      | CHAVE CNPJ                      *
      * SPARM-LIM-CH-FILIAL-CONTR    | CHAVE FILIAL                    *
      * SPARM-LIM-CH-CTRL-CONTR      | CHAVE CONTROLE                  *
      * SPARM-LIM-CH-PRODUTO         | CHAVE CODIGO PRODUTO            *
      * SPARM-LIM-RAZAO-SOCIAL       | RAZAO SOCIAL                    *
      * SPARM-LIM-CREDITO            | LIMITE CREDITO                  *
      * SPARM-LIM-DT-FIM-VL-CRED     | VALIDADE CREDITO                *
      * SPARM-LIM-STATUS             | DOMINIO APROVACAO:              *
      *                              |    P - PENDENTE DE APROVACAO    *
      *                              |    R - APROVADO PELO REGIONAL   *
      *                              |    N - RECUSADO                 *
      *                              |    S - APROVADO PELO BACKOFFICE *
      * SPARM-LIM-DT-INCLUSAO        | DATA INCLUSAO DO LIMITE         *
      * SPARM-LIM-USUARIO            | USUARIO DE INCLUSAO DO LIMITE   *
      * SPARM-LIM-PARECER-REG        | PARECER DO DIRETOR REGIONAL     *
      * SPARM-LIM-DT-PARE-REG        | DATA DO PARECER REGIONAL        *
      * SPARM-LIM-USUARIO-REG        | COD. USUARIO REGIONAL DO PARECER*
      * SPARM-LIM-PARECER-BCK        | PARECER DO BACKOFFICE           *
      * SPARM-LIM-DT-PARE-BCK        | DATA DO PARECER BACKOFFICE      *
      * SPARM-LIM-USUARIO-BCK        | COD. USUARIO BACKOFFICE PARECER *
      *----------------------------------------------------------------*
      * DATA       | AUTOR           | DESCRICAO / MANUTENCAO          *
      *----------------------------------------------------------------*
      *            |                 |                                 *
      *----------------------------------------------------------------*
       01  SPARM-LIM-TECNICO.
           05 SPARM-LIM-CH.
              10 SPARM-LIM-CH-AGENCIA            PIC 9(005).
              10 SPARM-LIM-CH-AGENCIAR REDEFINES
                 SPARM-LIM-CH-AGENCIA.
                 15 SPARM-LIM-CH-AGENCIA4        PIC 9(004).
                 15 SPARM-LIM-CH-DIGITO          PIC 9(001).
7C2511*         10 SPARM-LIM-CH-CNPJ-CONTR         PIC 9(008).
7C2511*         10 SPARM-LIM-CH-FILIAL-CONTR       PIC 9(004).
7C2511          10 SPARM-LIM-CH-CNPJ-CONTR         PIC X(008).
7C2511          10 SPARM-LIM-CH-FILIAL-CONTR       PIC X(004).
              10 SPARM-LIM-CH-CTRL-CONTR         PIC 9(002).
              10 SPARM-LIM-CH-PRODUTO            PIC 9(003).
              10 SPARM-LIM-FILLER                PIC X(003).
           05 SPARM-LIM-RAZAO-SOCIAL             PIC X(040).
           05 SPARM-LIM-CREDITO                  PIC 9(015)V99.
           05 SPARM-LIM-DT-FIM-VL-CRED           PIC 9(008).
           05 SPARM-LIM-STATUS                   PIC X(001).
           05 SPARM-LIM-DT-INCLUSAO              PIC 9(008).
           05 SPARM-LIM-USUARIO-INCL             PIC X(009).
           05 SPARM-LIM-PARECER-REG              PIC X(040).
           05 SPARM-LIM-DT-PARE-REG              PIC 9(008).
           05 SPARM-LIM-USUARIO-REG              PIC X(009).
           05 SPARM-LIM-PARECER-BCK              PIC X(040).
           05 SPARM-LIM-DT-PARE-BCK              PIC 9(008).
           05 SPARM-LIM-USUARIO-BCK              PIC X(009).
           05 SPARM-LIM-FILLER-02                PIC X(028).
