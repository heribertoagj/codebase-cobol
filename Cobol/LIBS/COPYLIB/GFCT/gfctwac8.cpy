      ******************************************************************
      * NOME DA INC - GFCTWAC8 - GFCT5545                              *
      * DESCRICAO   - TARIFAS - EXTRATO ANUAL DE TARIFAS               *
      *               COMBO DE ANO DE SOLIC PREV EMISS CORREIO EXTRATO *
      *               COMUNICACAO WEB X IMS - ENTRADA                  *
      * TAMANHO     - 0100 (GERAL)                                     *
      *             - 0042 (FIXO)                                      *
      * DATA        - 10/2008                                          *
      * RESPONSAVEL - VALERIA TORQUATO - SONDA PROCWORK - 4506-2349    *
      ******************************************************************
      *
       01  GFCTWAC8-ENTRADA.
           03  GFCTWAC8-LL                       PIC S9(004) COMP.
           03  GFCTWAC8-ZZ                       PIC S9(004) COMP.
           03  GFCTWAC8-TRANSACAO                PIC  X(009).
           03  GFCTWAC8-FUNCAO                   PIC  X(008).
           03  GFCTWAC8-DADOS-RST.
               05  GFCTWAC8-QTDE-OCOR            PIC  9(003).
               05  GFCTWAC8-QTDE-TOT-REG         PIC  9(006).
               05  GFCTWAC8-FUNC-BDSCO           PIC  X(007).
               05  GFCTWAC8-FILTRO.
                   07  GFCTWAC8-FTR-ACAO         PIC  X(001).
      *    'H' - CONSULTA     = HISTORICO
      *    'V' - CANCELAMENTO = VIGENTE
               05  GFCTWAC8-PONTEIRO.
                   07  GFCTWAC8-PTR-ANO          PIC  9(004).
               05  GFCTWAC8-FIM                  PIC  X(001).
           03  FILLER                            PIC  X(057).
