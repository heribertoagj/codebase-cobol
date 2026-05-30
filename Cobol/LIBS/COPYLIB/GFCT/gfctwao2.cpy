      *****************************************************************
      *               COMUNICACAO WEB X IMS                 - ENTRADA *
      * NOME DA INC - GFCTWAO2                                        *
      * DESCRICAO   - GESTAO - DEMONSTRATIVO DE EVENTOS CORRENTISTAS  *
      *                        LISTA DE EVENTOS            - GFCT0982 *
      * TAMANHO     - 150                                             *
      * DATA        - OUT/2010                                        *
      * RESPONSAVEL - FRANCISCO LOURENCO              - SONDAPROCWORK *
      *****************************************************************
      *
       01  WAO2-ENTRADA.
           03  WAO2-LL                      PIC S9(004) COMP.
           03  WAO2-ZZ                      PIC S9(004) COMP.
           03  WAO2-TRANSACAO               PIC  X(009).
           03  WAO2-FUNCAO                  PIC  X(008).
           03  WAO2-DADOS-RST.
               05  WAO2-FILTRO.
                   07  WAO2-FLT-AGENCIA     PIC  9(005).
                   07  WAO2-FLT-CONTA       PIC  9(013).
                   07  WAO2-FLT-PERIODO-INI PIC  X(010).
                   07  WAO2-FLT-PERIODO-FIM PIC  X(010).
                   07  WAO2-FLT-COD-TARIFA  PIC  9(005).
               05  WAO2-PONTEIRO.
                   07  WAO2-PTR-OCOR-ATUAL  PIC  9(007).
                   07  WAO2-PTR-OCOR-TOTAL  PIC  9(007).
                   07  WAO2-PTR-CONTINUA    PIC  X(001).
               05  WAO2-QTDE-OCOR           PIC  9(003).
               05  WAO2-QTDE-TOT-REG        PIC  9(006).
               05  WAO2-DEPTO-BDSCO         PIC  9(005).
               05  WAO2-FUNC-BDSCO          PIC  X(007).
               05  WAO2-FIM                 PIC  X(001).
           03  FILLER                       PIC  X(049).
