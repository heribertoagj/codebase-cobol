      *****************************************************************
      * NOME DA INC - GFCTWALA                                        *
      * DESCRICAO   - FLEXZ PARA AG./CONTA - LISTA -  CANCEL PEDIDOS  *
      *               POR LOTE                                        *
      *               COMUNICACAO WEB X IMS - SAIDA - GFCT5275        *
      * TAMANHO     - 1010                                            *
      * WALA-LL     - 0102                                            *
      * DATA        - 08.02.2010                                      *
      * RESPONSAVEL - VANI NUNES - PROCWORK                           *
      *****************************************************************
      *
       01  WALA-SAIDA.
           03  WALA-LL                            PIC S9(004) COMP.
           03  WALA-ZZ                            PIC S9(004) COMP.
           03  WALA-TRANSACAO                     PIC  X(009).
           03  WALA-FUNCAO                        PIC  X(008).
           03  WALA-DADOS-RST.
               05  WALA-QTDE-OCOR                 PIC  9(003).
               05  WALA-QTDE-TOT-REG              PIC  9(006).
               05  WALA-FUNC-BDSCO                PIC  X(007).
               05  WALA-FILTRO.
                   07  WALA-AGENCIA-FIL           PIC  9(005).
                   07  WALA-CONTA-FIL             PIC  9(013).
                   07  WALA-TARIFA-FIL            PIC  9(005).
                   07  WALA-CONJ-SERV-FIL         PIC  9(003).
                   07  WALA-COD-LOTE-FIL          PIC  9(011).
                   07  WALA-CANC-TODAS-FIL        PIC  X(001).
               05  WALA-PONTEIRO.
                   07  WALA-AGENCIA-PONT          PIC  9(005).
                   07  WALA-CONTA-PONT            PIC  9(013).
                   07  WALA-TARIFA-PONT           PIC  9(005).
                   07  WALA-CONJ-SERV-PONT        PIC  9(003).
                   07  WALA-COD-LOTE-PONT         PIC  9(011).
                   07  WALA-TIMESTAMP-PONT        PIC  X(026).
           03  WALA-ERRO                          PIC  9(001).
           03  WALA-COD-MSG-ERRO                  PIC  9(007).
           03  WALA-COD-SQL-ERRO                  PIC  9(003).
           03  WALA-DESC-MSG-ERRO                 PIC  X(079).
           03  WALA-FIM                           PIC  X(001).
           03  FILLER                             PIC  X(067).
           03  WALA-DADOS.
               05  FILLER OCCURS 7.
                   07  WALA-COD-TARIFA            PIC  9(005).
                   07  WALA-TIMESTAMP             PIC  X(026).
                   07  WALA-FRANQUIA              PIC  9(003).
                   07  WALA-PERIODO               PIC  9(001).
                   07  WALA-VR-NEGOCIADO          PIC  9(006)V99.
                   07  WALA-PERC-DESCTO           PIC  9(003)V9(04).
                   07  WALA-DIAS-RETENCAO         PIC  9(002).
                   07  WALA-DIA-AGEND             PIC  9(002).
                   07  WALA-INIC-VIG-FLEX         PIC  X(010).
                   07  WALA-FIM-VIG-FLEX          PIC  X(010).
                   07  WALA-SITUACAO-FLEX         PIC  9(002).
                   07  WALA-JUNCAO-SOLIC          PIC  9(005).
                   07  WALA-NRO-LOTE              PIC  9(011).
                   07  WALA-DT-SOLICT             PIC  X(010).
