      *****************************************************************
      * NOME DA INC - I#GFCTKO                                        *
      * DESCRICAO   - MODULO VERIFICAÇ O PERMISSAO USUARIO FLEXIBILIZ *
      *               COMUNICACAO WEB X IMS - SAIDA                   *
      * TAMANHO     - 0300                                            *
      * DATA        - FEV/2006                                        *
      * RESPONSAVEL - VALERIA - PROCWORK - TEL 41524352 R 210         *
      *****************************************************************
      *
       01  GFCTKO-SAIDA.
           03  GFCTKO-LL                            PIC S9(004) COMP.
           03  GFCTKO-ZZ                            PIC S9(004) COMP.
           03  GFCTKO-TRANSACAO                     PIC  X(009).
           03  GFCTKO-FUNCAO                        PIC  X(008).
           03  GFCTKO-CHAVE.
               05 GFCTKO-AGENCIA-FL                 PIC  9(005).
               05 GFCTKO-CONTA-FL                   PIC  9(007).
           03  GFCTKO-CJUNC-DEPDC-FUNC              PIC  9(005).
           03  GFCTKO-CIDTFD-DEPDC-FUNC             PIC  X(002).
           03  GFCTKO-QTDE-OCOR                     PIC  9(003).
           03  GFCTKO-QTDE-TOT-REG                  PIC  9(006).
           03  GFCTKO-FUNC-BDSCO                    PIC  X(007).
           03  GFCTKO-TERMINAL                      PIC  X(008).
           03  GFCTKO-NOME-CLIENTE                  PIC  X(040).
           03  GFCTKO-FIM                           PIC  X(001).
           03  GFCTKO-MASTER                        PIC  X(001).
           03  GFCTKO-IND-OBRIG-AGENCIA             PIC  9(001).
      *        0 - NAO EH OBRIGATORIO A DIGITACAO DA AGENCIA
      *        1 - EH OBRIGATORIO A DIGITACAO DA AGENCIA
           03  GFCTKO-IND-OBRIG-CONTA               PIC  9(001).
      *        0 - NAO EH OBRIGATORIO A DIGITACAO DA CONTA
      *        1 - EH OBRIGATORIO A DIGITACAO DA CONTA
           03  GFCTKO-ERRO                          PIC  9(001).
           03  GFCTKO-COD-MSG-ERRO                  PIC  9(007).
           03  GFCTKO-COD-SQL-ERRO                  PIC  9(003).
           03  GFCTKO-DESC-MSG-ERRO                 PIC  X(079).
           03  FILLER                               PIC  X(102).
