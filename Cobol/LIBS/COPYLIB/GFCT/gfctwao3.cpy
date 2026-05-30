      *****************************************************************
      *               COMUNICACAO WEB X IMS                   - SAIDA *
      * NOME DA INC - GFCTWAO3                                        *
      * DESCRICAO   - GESTAO - DEMONSTRATIVO DE EVENTOS CORRENTISTAS  *
      *                        LISTA DE EVENTOS            - GFCT0982 *
      * TAMANHO     - 1010                                            *
      *                205 (FIX0)                                     *
      *                115 (VARIAVEL; 7X)                             *
      * DATA        - OUT/2010                                        *
      * RESPONSAVEL - FRANCISCO LOURENCO              - SONDAPROCWORK *
      *****************************************************************
      *
       01  WAO3-SAIDA.
           03  WAO3-LL                      PIC S9(004) COMP.
           03  WAO3-ZZ                      PIC S9(004) COMP.
           03  WAO3-TRANSACAO               PIC  X(009).
           03  WAO3-FUNCAO                  PIC  X(008).
           03  WAO3-FILTRO.
               05  WAO3-FLT-AGENCIA         PIC  9(005).
               05  WAO3-FLT-CONTA           PIC  9(013).
               05  WAO3-FLT-PERIODO-INI     PIC  X(010).
               05  WAO3-FLT-PERIODO-FIM     PIC  X(010).
               05  WAO3-FLT-COD-TARIFA      PIC  9(005).
           03  WAO3-PONTEIRO.
               05  WAO3-PTR-OCOR-ATUAL      PIC  9(007).
               05  WAO3-PTR-OCOR-TOTAL      PIC  9(007).
               05  WAO3-PTR-CONTINUA        PIC  X(001).
           03  WAO3-QTDE-OCOR               PIC  9(003).
           03  WAO3-QTDE-TOT-REG            PIC  9(006).
           03  WAO3-DEPTO-BDSCO             PIC  9(005).
           03  WAO3-FUNC-BDSCO              PIC  X(007).
           03  WAO3-FIM                     PIC  X(001).
           03  WAO3-ERRO                    PIC  9(001).
           03  WAO3-COD-MSG-ERRO            PIC  9(007).
           03  WAO3-COD-SQL-ERRO            PIC  9(003).
           03  WAO3-DESC-MSG-ERRO           PIC  X(079).
           03  FILLER                       PIC  X(014).
           03  WAO3-DADOS.
               05  FILLER                   OCCURS 7.
                   07  WAO3-SEL             PIC  X(001).
                   07  WAO3-DT-EVENTO       PIC  X(010).
                   07  WAO3-CD-TARIFA       PIC  9(005).
                   07  WAO3-QT-EVENTOS      PIC  9(006).
                   07  WAO3-VL-POTENCIAL    PIC  9(009)V99.
                   07  WAO3-VL-CALCULADO    PIC  9(009)V99.
                   07  WAO3-VL-RECEBIDO     PIC  9(009)V99.
                   07  WAO3-VL-RECEBER      PIC  9(009)V99.
                   07  WAO3-VL-ESTORNADO    PIC  9(009)V99.
                   07  WAO3-VL-PERDA        PIC  9(008)V99.
                   07  WAO3-ROTINA          PIC  X(004).
                   07  WAO3-DT-MOVIMENTO    PIC  X(010).
                   07  WAO3-ARQUIVO         PIC  9(002).
                   07  WAO3-SEQ-ARQUIVO     PIC  9(008).
                   07  WAO3-SEQ-TELA        PIC  9(004).
