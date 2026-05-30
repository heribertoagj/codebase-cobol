      *****************************************************************
      * NOME DA INC - I#GFCTO3                                        *
      * DESCRICAO   - COMBO - TIPO AGRUPAMENTO - INCLUS O             *
      *               COMUNICACAO IMS X WEB - ENTRADA                 *
      * TAMANHO     - 1010                                            *
      * GFCTO3-LL   - 1010                                            *
      * DATA        - ABRIL/2006                                      *
      * RESPONSAVEL - MARCELO CREMM   - PROCWORK                      *
      * CONTEMPLAR  TODOS AGRUP. FIXOS MENOS EMPRESA.                 *
      *****************************************************************
       01  GFCTO3-ENTRADA.
           03 GFCTO3-LL                            PIC S9(004) COMP.
           03 GFCTO3-ZZ                            PIC S9(004) COMP.
           03 GFCTO3-TRANSACAO                     PIC  X(009).
           03 GFCTO3-FUNCAO                        PIC  X(008).
           03 GFCTO3-DADOS-REST.
              05 GFCTO3-QTDE-OCOR                  PIC  9(003).
              05 GFCTO3-QTDE-TOT-REG               PIC  9(006).
              05 GFCTO3-FUNC-BDSCO                 PIC  X(007).
              05 GFCTO3-TERMINAL                   PIC  X(008).
              05 GFCTO3-FILTRO.
                 07 GFCTO3-FILT-AGENCIA            PIC  9(005).
                 07 GFCTO3-FILT-CONTA              PIC  9(013).
                 07 GFCTO3-FILT-PACOTE             PIC  9(005).
              05 GFCTO3-PONT-AGENCIA               PIC  9(005).
              05 GFCTO3-PONT-CONTA                 PIC  9(013).
              05 GFCTO3-PONT-PACOTE                PIC  9(005).
              05 GFCTO3-PONT-CAGPTO                PIC  9(003).
              05 GFCTO3-FIM                        PIC  X(001).
           03 GFCTO3-FILLER                        PIC  X(915).
