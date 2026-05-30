      ******************************************************************
      * NOME DA INC - GFCTWAC9 - GFCT5545                              *
      * DESCRICAO   - TARIFAS - EXTRATO ANUAL DE TARIFAS               *
      *               COMBO DE ANO DE SOLIC PREV EMISS CORREIO EXTRATO *
      *               COMUNICACAO WEB X IMS - SAIDA                    *
      * TAMANHO     - 1010 (GERAL)                                     *
      *             - 0810 (PARTE FIXA)                                *
      *             - 0200 (PARTE VARIAVEL)                            *
      *             - 0004 (CADA OCORRENCIA)                           *
      * DATA        - 10/2008                                          *
      * RESPONSAVEL - VALERIA TORQUATO - SONDA PROCWORK - 4506-2349    *
      ******************************************************************
      *
       01  GFCTWAC9-SAIDA.
           03  GFCTWAC9-LL                       PIC S9(004) COMP.
           03  GFCTWAC9-ZZ                       PIC S9(004) COMP.
           03  GFCTWAC9-TRANSACAO                PIC  X(009).
           03  GFCTWAC9-FUNCAO                   PIC  X(008).
           03  GFCTWAC9-QTDE-OCOR                PIC  9(003).
           03  GFCTWAC9-QTDE-TOT-REG             PIC  9(006).
           03  GFCTWAC9-FUNC-BDSCO               PIC  X(007).
           03  GFCTWAC9-FILTRO.
               05  GFCTWAC9-FTR-ACAO             PIC  X(001).
      *    'H' - CONSULTA     = HISTORICO
      *    'V' - CANCELAMENTO = VIGENTE
           03  GFCTWAC9-PONTEIRO.
               05  GFCTWAC9-PTR-ANO              PIC  9(004).
           03  GFCTWAC9-ERRO                     PIC  9(001).
           03  GFCTWAC9-COD-MSG-ERRO             PIC  9(007).
           03  GFCTWAC9-COD-SQL-ERRO             PIC  9(003).
           03  GFCTWAC9-DESC-MSG-ERRO            PIC  X(079).
           03  GFCTWAC9-FIM                      PIC  X(001).
           03  FILLER                            PIC  X(677).
           03  GFCTWAC9-DADOS                    OCCURS 50.
               05  GFCTWAC9-COD-ANO              PIC  9(004).
