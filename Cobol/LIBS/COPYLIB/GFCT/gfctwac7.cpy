      ******************************************************************
      * NOME DA INC - GFCTWAC7 - GFCT0543                              *
      * DESCRICAO   - TARIFAS - EXTRATO ANUAL DE TARIFAS               *
      *               SOLICITACAO PREVIA VALIDACAO BASE DADOS EXTRATO  *
      *               COMUNICACAO WEB X IMS - SAIDA                    *
      * TAMANHO     - 1010 (GERAL)                                     *
      * DATA        - 10/2008                                          *
      * RESPONSAVEL - VALERIA TORQUATO - SONDA PROCWORK - 4506-2349    *
      ******************************************************************
      *
       01  GFCTWAC7-SAIDA.
           03  GFCTWAC7-LL                       PIC S9(004) COMP.
           03  GFCTWAC7-ZZ                       PIC S9(004) COMP.
           03  GFCTWAC7-TRANSACAO                PIC  X(009).
           03  GFCTWAC7-FUNCAO                   PIC  X(008).
           03  GFCTWAC7-QTDE-OCOR                PIC  9(003).
           03  GFCTWAC7-QTDE-TOT-REG             PIC  9(006).
           03  GFCTWAC7-FUNC-BDSCO               PIC  X(007).
           03  GFCTWAC7-ERRO                     PIC  9(001).
           03  GFCTWAC7-COD-MSG-ERRO             PIC  9(007).
           03  GFCTWAC7-COD-SQL-ERRO             PIC  9(003).
           03  GFCTWAC7-DESC-MSG-ERRO            PIC  X(079).
           03  GFCTWAC7-FIM                      PIC  X(001).
           03  GFCTWAC7-DADOS.
               05  GFCTWAC7-COD-MES-SOLIC        PIC  9(002).
               05  GFCTWAC7-COD-AGENC-SOLIC      PIC  9(005).
           03  FILLER                            PIC  X(875).
