      *****************************************************************
      * NOME DA INC - GFCTWANP                                        *
      * DESCRICAO   - FLEXIBILIZACAO PARA AG/CONTA - DEFER DETALHE    *
      *               COMUNICACAO WEB X IMS        - SAIDA (0877)     *
      * TAMANHO     - 1010                                            *
      * GFCTNP-LL   - 1010                                            *
      * DATA        - 11.06.2010                                      *
      * RESPONSAVEL - CIBELE  - PROCWORK                              *
      *****************************************************************
      *
       01  GFCTNP-SAIDA.
           03  GFCTNP-LL                            PIC S9(004) COMP.
           03  GFCTNP-ZZ                            PIC S9(004) COMP.
           03  GFCTNP-TRANSACAO                     PIC  X(009).
           03  GFCTNP-FUNCAO                        PIC  X(008).
           03  GFCTNP-QTDE-OCOR                     PIC  9(003).
           03  GFCTNP-QTDE-TOT-REG                  PIC  9(006).
           03  GFCTNP-FUNC-BDSCO                    PIC  X(007).
           03  GFCTNP-ERRO                          PIC  9(001).
           03  GFCTNP-COD-MSG-ERRO                  PIC  9(007).
           03  GFCTNP-COD-SQL-ERRO                  PIC  9(003).
           03  GFCTNP-DESC-MSG-ERRO                 PIC  X(079).
           03  GFCTNP-FIM                           PIC  X(001).
           03  FILLER                               PIC  X(032).
           03  GFCTNP-FILTRO.
               05  GFCTNP-AGENCIA-FIL           PIC  9(005).
               05  GFCTNP-CONTA-FIL             PIC  9(013).
               05  GFCTNP-DATA-FIL              PIC  X(010).
               05  GFCTNP-LOTE-FIL              PIC  9(011).
               05  GFCTNP-TIMESTAMP-FIL         PIC  X(026).
               05  GFCTNP-CONJ-SERV-FIL         PIC  9(003).
           03  GFCTNP-PONTEIRO.
               05  GFCTNP-TARIFA-PONT           PIC  9(005).
           03  GFCTNP-DEFER-LOTE OCCURS 7 TIMES.
               05  GFCTNP-COD-TARIFA            PIC  9(005).
               05  GFCTNP-NOME-TARIFA           PIC  X(030).
               05  GFCTNP-FRANQUIA              PIC  9(003).
               05  GFCTNP-PERIODO               PIC  9(001).
               05  GFCTNP-PERC-DESCTO           PIC  9(007)V9(04).
               05  GFCTNP-PERC-SOLICITADO       PIC  9(007)V9(04).
               05  GFCTNP-VR-SOLICITADO         PIC  9(009)V99.
               05  GFCTNP-VR-ATUAL              PIC  9(009)V99.
               05  GFCTNP-VR-INTEGRAL           PIC  9(009)V99.
               05  GFCTNP-DIAS-RETENCAO         PIC  9(002).
               05  GFCTNP-VOL-DEP-MES           PIC  9(009).
               05  GFCTNP-DIA-AGEND             PIC  9(002).
               05  GFCTNP-COD-MSG-FLEX-VIG      PIC  X(001).
      * 0 = OK
      * 1 = TARIFA COMBINADA - CALCULO NAO REALIZADO
      * 2 = NAO EXISTE FLEXIBILIZACAO VIGENTE
      * 3 = FLEXIBILIZACAO VIGENTE NAO CONTEMPLA PERCENTUAL OU VALOR
               05  GFCTNP-COD-MSG-VLR-INTEG     PIC  X(001).
      * 0 = OK
      * 1 = TARIFA COMBINADA - CALCULO NAO REALIZADO
               05  GFCTNP-PERIODO-COBR          PIC  9(001).
      * 1 = DIARIO
      * 2 = SEMANAL
      * 3 = QUINZENAL
      * 4 = MENSAL
               05  GFCTNP-INIC-PER-COBR         PIC  9(001).
      * 1 - DOMINGO
      * 2 - SEGUNDA-FEIRA
      * 3 - TERCA-FEIRA
      * 4 - QUARTA-FEIRA
      * 5 - QUINTA-FEIRA
      * 6 - SEXTA-FEIRA
      * 7 - SABADO
