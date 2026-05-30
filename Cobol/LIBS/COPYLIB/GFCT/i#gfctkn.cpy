      *****************************************************************
      * NOME DA INC - I#GFCTKN                                        *
      * DESCRICAO   - MODULO VERIFICAÇ O PERMISSAO USUARIO FLEXIBILIZ *
      *               COMUNICACAO WEB X IMS - ENTRADA                 *
      * TAMANHO     - 100                                             *
      * DATA        - FEV/2006                                        *
      * RESPONSAVEL - VALERIA - PROCWORK - TEL 41524352 R 210         *
      *****************************************************************
      *
       01  GFCTKN-ENTRADA.
           03  GFCTKN-LL                            PIC S9(004) COMP.
           03  GFCTKN-ZZ                            PIC S9(004) COMP.
           03  GFCTKN-TRANSACAO                     PIC  X(009).
           03  GFCTKN-FUNCAO                        PIC  X(008).
           03  GFCTKN-DADOS-RST.
               05  GFCTKN-CHAVE.
                   07 GFCTKN-AGENCIA-FL             PIC  9(005).
                   07 GFCTKN-CONTA-FL               PIC  9(007).
               05  GFCTKN-QTDE-OCOR                 PIC  9(003).
               05  GFCTKN-QTDE-TOT-REG              PIC  9(006).
               05  GFCTKN-FUNC-BDSCO                PIC  X(007).
               05  GFCTKN-TERMINAL                  PIC  X(008).
               05  GFCTKN-MASTER                    PIC  X(001).
               05  GFCTKN-CJUNC-DEPDC-FUNC          PIC  9(005).
               05  GFCTKN-CIDTFD-DEPDC-FUNC         PIC  X(002).
               05  GFCTKN-FIM                       PIC  X(001).
               05  GFCTKN-IND-OBRIG-AGENCIA         PIC  9(001).
      *            0 - NAO EH OBRIGATORIO A DIGITACAO DA AGENCIA
      *            1 - EH OBRIGATORIO A DIGITACAO DA AGENCIA
               05  GFCTKN-IND-OBRIG-CONTA           PIC  9(001).
      *            0 - NAO EH OBRIGATORIO A DIGITACAO DA CONTA
      *            1 - EH OBRIGATORIO A DIGITACAO DA CONTA
               05  FILLER                           PIC  X(032).
