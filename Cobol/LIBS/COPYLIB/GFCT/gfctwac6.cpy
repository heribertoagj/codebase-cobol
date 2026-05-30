      ******************************************************************
      * NOME DA INC - GFCTWAC6 - GFCT0543                              *
      * DESCRICAO   - TARIFAS - EXTRATO ANUAL DE TARIFAS               *
      *               SOLICITACAO PREVIA VALIDACAO BASE DADOS EXTRATO  *
      *               COMUNICACAO WEB X IMS - ENTRADA                  *
      * TAMANHO     - 0100 (GERAL)                                     *
      * DATA        - 10/2008                                          *
      * RESPONSAVEL - VALERIA TORQUATO - SONDA PROCWORK - 4506-2349    *
      ******************************************************************
      *
       01  GFCTWAC6-ENTRADA.
           03  GFCTWAC6-LL                       PIC S9(004) COMP.
           03  GFCTWAC6-ZZ                       PIC S9(004) COMP.
           03  GFCTWAC6-TRANSACAO                PIC  X(009).
           03  GFCTWAC6-FUNCAO                   PIC  X(008).
           03  GFCTWAC6-DADOS-RST.
               05  GFCTWAC6-QTDE-OCOR            PIC  9(003).
               05  GFCTWAC6-QTDE-TOT-REG         PIC  9(006).
               05  GFCTWAC6-FUNC-BDSCO           PIC  X(007).
               05  GFCTWAC6-FIM                  PIC  X(001).
               05  GFCTWAC6-DADOS.
                   07  GFCTWAC6-COD-MES-SOLIC    PIC  9(002).
                   07  GFCTWAC6-COD-AGENC-SOLIC  PIC  9(005).
           03  FILLER                            PIC  X(055).
