      *****************************************************************
      * NOME DA INC - I#GFCTNO                                        *
      * DESCRICAO   - FLEXIBILIZACAO PARA AGRUPAMENTO - LISTA DEFERIM *
      *               COMUNICACAO WEB X IMS           - SAIDA  (0425) *
      * TAMANHO     - 1010                                            *
      * GFCTNO-LL   - 1010                                            *
      * DATA        - 25.03.2006                                      *
      * RESPONSAVEL - VALERIA - PROCWORK                              *
      *****************************************************************
      *
       01  GFCTNO-SAIDA.
           03  GFCTNO-LL                            PIC S9(004) COMP.
           03  GFCTNO-ZZ                            PIC S9(004) COMP.
           03  GFCTNO-TRANSACAO                     PIC  X(009).
           03  GFCTNO-FUNCAO                        PIC  X(008).
           03  GFCTNO-DADOS-RST.
               05  GFCTNO-QTDE-OCOR                 PIC  9(003).
               05  GFCTNO-QTDE-TOT-REG              PIC  9(006).
               05  GFCTNO-FUNC-BDSCO                PIC  X(007).
               05  GFCTNO-FILTRO.
                   07  GFCTNO-AGRUPAMENTO-FIL       PIC  9(003).
                   07  GFCTNO-DATA-FIL              PIC  X(010).
               05  GFCTNO-PONTEIRO.
                   07  GFCTNO-AGRUPAMENTO-PONT      PIC  9(003).
                   07  GFCTNO-TARIFA-PONT           PIC  9(005).
                   07  GFCTNO-TIMESTAMP-PONT        PIC  X(026).
                   07  GFCTNO-SEQ-PONT              PIC  9(002).
                   07  GFCTNO-DATA-PONT             PIC  X(010).
           03  GFCTNO-ERRO                          PIC  9(001).
           03  GFCTNO-COD-MSG-ERRO                  PIC  9(007).
           03  GFCTNO-COD-SQL-ERRO                  PIC  9(003).
           03  GFCTNO-DESC-MSG-ERRO                 PIC  X(079).
           03  GFCTNO-FIM                           PIC  X(001).
           03  FILLER                               PIC  X(091).
           03  GFCTNO-DADOS OCCURS 4.
               05  GFCTNO-CGC-CPF                   PIC  X(009).
               05  GFCTNO-FILIAL                    PIC  X(004).
               05  GFCTNO-CONTROLE                  PIC  X(002).
               05  GFCTNO-AGENCIA                   PIC  9(005).
               05  GFCTNO-POSTO                     PIC  9(005).
               05  GFCTNO-SEGMENTO                  PIC  9(003).
               05  GFCTNO-MUNICIPIO                 PIC  9(007).
               05  GFCTNO-UF                        PIC  X(002).
               05  GFCTNO-DESCR-AGPTO               PIC  X(030).
               05  GFCTNO-COD-TARIFA                PIC  9(005).
               05  GFCTNO-DESCR-TARIFA              PIC  X(015).
               05  GFCTNO-DATA-SOLIC                PIC  X(010).
               05  GFCTNO-GRUPO-CTBIL               PIC  9(003).
               05  GFCTNO-SUB-GRUPO-CTBIL           PIC  9(003).
               05  GFCTNO-DESC-CTBIL                PIC  X(040).
               05  GFCTNO-TIMESTAMP                 PIC  X(026).
               05  GFCTNO-SEQUENCIA                 PIC  9(002).
               05  GFCTNO-ERRO-OCORR                PIC  9(001).
               05  GFCTNO-COD-MSG-ERRO-OCORR        PIC  9(007).
               05  GFCTNO-COD-SQL-ERRO-OCORR        PIC  9(003).

