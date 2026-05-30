      *****************************************************************
      * NOME DA INC - I#GFCTP2                                        *
      * DESCRICAO   - TEXT DE NOME DO CORRESPONDENTE BANCARIO COM     *
      *               AGRUPAMENTO - COMUNICACAO WEB X IMS - ENTRADA   *
      * TAMANHO     - 100                                             *
      * DATA        - 04.04.2006                                      *
      * RESPONSAVEL - RICARDO PEREIRA - PROCWORK                      *
      *****************************************************************
      *
       01  GFCTP2-ENTRADA.
           03  GFCTP2-LL                            PIC S9(004) COMP.
           03  GFCTP2-ZZ                            PIC S9(004) COMP.
           03  GFCTP2-TRANSACAO                     PIC  X(009).
           03  GFCTP2-FUNCAO                        PIC  X(008).
           03  GFCTP2-DADOS-RST.
               05  GFCTP2-FILTRO.
                   07  GFCTP2-FILTRO-AGRUP          PIC  9(003).
                   07  GFCTP2-FILTRO-AGENCIA        PIC  9(005).
                   07  GFCTP2-FILTRO-CORRESP        PIC  9(005).
               05  GFCTP2-QTDE-OCOR                 PIC  9(007).
               05  GFCTP2-QTDE-TOT-REG              PIC  9(009).
               05  GFCTP2-FUNC-BDSCO                PIC  X(007).
               05  GFCTP2-FIM                       PIC  X(001).
               05  FILLER                           PIC  X(042).
