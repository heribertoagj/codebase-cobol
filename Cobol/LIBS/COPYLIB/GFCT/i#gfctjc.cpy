      *****************************************************************
      * NOME DA INC - I#GFCTJC                                        *
      * DESCRICAO   - FLEXZ PARA AG./CONTA - LISTA    CANCEL PEDIDOS  *
      *               COMUNICACAO WEB X IMS - SAIDA - GFCT0405        *
      * TAMANHO     - 1010                                            *
      * GFCTJC-LL   - 0102                                            *
      * DATA        - 28.12.2005                                      *
      * RESPONSAVEL - VALERIA - PROCWORK                              *
      *****************************************************************
      *
       01  GFCTJC-SAIDA.
           03  GFCTJC-LL                            PIC S9(004) COMP.
           03  GFCTJC-ZZ                            PIC S9(004) COMP.
           03  GFCTJC-TRANSACAO                     PIC  X(009).
           03  GFCTJC-FUNCAO                        PIC  X(008).
           03  GFCTJC-DADOS-RST.
               05  GFCTJC-QTDE-OCOR                 PIC  9(003).
               05  GFCTJC-QTDE-TOT-REG              PIC  9(006).
               05  GFCTJC-FUNC-BDSCO                PIC  X(007).
               05  GFCTJC-FILTRO.
                   07  GFCTJC-AGENCIA-FIL           PIC  9(005).
                   07  GFCTJC-CONTA-FIL             PIC  9(013).
                   07  GFCTJC-TARIFA-FIL            PIC  9(005).
               05  GFCTJC-PONTEIRO.
                   07  GFCTJC-AGENCIA-PONT          PIC  9(005).
                   07  GFCTJC-CONTA-PONT            PIC  9(013).
                   07  GFCTJC-TARIFA-PONT           PIC  9(005).
                   07  GFCTJC-TIMESTAMP-PONT        PIC  X(026).
           03  GFCTJC-ERRO                          PIC  9(001).
           03  GFCTJC-COD-MSG-ERRO                  PIC  9(007).
           03  GFCTJC-COD-SQL-ERRO                  PIC  9(003).
           03  GFCTJC-DESC-MSG-ERRO                 PIC  X(079).
           03  GFCTJC-FIM                           PIC  X(001).
           03  FILLER                               PIC  X(096).
           03  GFCTJC-DADOS.
               05  FILLER OCCURS 7.
                   07  GFCTJC-COD-TARIFA            PIC  9(005).
                   07  GFCTJC-TIMESTAMP             PIC  X(026).
                   07  GFCTJC-FRANQUIA              PIC  9(003).
                   07  GFCTJC-PERIODO               PIC  9(001).
                   07  GFCTJC-VR-NEGOCIADO          PIC  9(006)V99.
                   07  GFCTJC-PERC-DESCTO           PIC  9(003)V9(04).
                   07  GFCTJC-DIAS-RETENCAO         PIC  9(002).
                   07  GFCTJC-DIA-AGEND             PIC  9(002).
                   07  GFCTJC-INIC-VIG-FLEX         PIC  X(010).
                   07  GFCTJC-FIM-VIG-FLEX          PIC  X(010).
                   07  GFCTJC-SITUACAO-FLEX         PIC  9(002).
                   07  GFCTJC-JUNCAO-SOLIC          PIC  9(005).
                   07  GFCTJC-NRO-LOTE              PIC  9(011).
                   07  GFCTJC-DT-SOLICT             PIC  X(010).
