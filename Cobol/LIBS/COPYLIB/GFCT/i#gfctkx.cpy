      *****************************************************************
      * NOME DA INC - I#GFCTKX                                        *
      * DESCRICAO   - FLEXIBILIZACAO PARA AG./CONTA - DEFERIMENTO     *
      *               COMUNICACAO WEB X IMS         - SAIDA  (0424)   *
      * TAMANHO     - 1010                                            *
      * GFCTKX-LL   - 1010                                            *
      * DATA        - 15.02.2006                                      *
      * RESPONSAVEL - VALERIA - PROCWORK                              *
      *****************************************************************
      *
       01  GFCTKX-SAIDA.
           03  GFCTKX-LL                            PIC S9(004) COMP.
           03  GFCTKX-ZZ                            PIC S9(004) COMP.
           03  GFCTKX-TRANSACAO                     PIC  X(009).
           03  GFCTKX-FUNCAO                        PIC  X(008).
           03  GFCTKX-DADOS-RST.
               05  GFCTKX-QTDE-OCOR                 PIC  9(003).
               05  GFCTKX-QTDE-TOT-REG              PIC  9(006).
               05  GFCTKX-FUNC-BDSCO                PIC  X(007).
               05  GFCTKX-FILTRO.
                   07  GFCTKX-AGENCIA-FIL           PIC  9(005).
                   07  GFCTKX-CONTA-FIL             PIC  9(013).
                   07  GFCTKX-DATA-FIL              PIC  X(010).
                   07  GFCTKX-LOTE-FIL              PIC  9(011).
                   07  GFCTKX-CONJ-SERV-FIL         PIC  9(003).
               05  GFCTKX-PONTEIRO.
                   07  GFCTKX-AGENCIA-PONT          PIC  9(005).
                   07  GFCTKX-CONTA-PONT            PIC  9(013).
                   07  GFCTKX-TARIFA-PONT           PIC  9(005).
                   07  GFCTKX-TIMESTAMP-PONT        PIC  X(026).
           03  GFCTKX-NOME-CLIENTE                  PIC  X(030).
           03  GFCTKX-ERRO                          PIC  9(001).
           03  GFCTKX-COD-MSG-ERRO                  PIC  9(007).
           03  GFCTKX-COD-SQL-ERRO                  PIC  9(003).
           03  GFCTKX-DESC-MSG-ERRO                 PIC  X(079).
           03  GFCTKX-FIM                           PIC  X(001).
           03  FILLER                               PIC  X(083).
           03  GFCTKX-DADOS.
               05  FILLER OCCURS 6.
                   07  GFCTKX-AGENCIA-LISTA         PIC  9(005).
                   07  GFCTKX-CONTA-LISTA           PIC  9(013).
                   07  GFCTKX-TARIFA-LISTA          PIC  9(005).
                   07  GFCTKX-TIMESTAMP             PIC  X(026).
                   07  GFCTKX-NOME-TARIFA           PIC  X(020).
                   07  GFCTKX-DATA-SOLIC            PIC  X(010).
                   07  GFCTKX-DEPEND                PIC  9(005).
                   07  GFCTKX-SITUACAO              PIC  X(018).
                   07  GFCTKX-LOTE                  PIC  9(011).
