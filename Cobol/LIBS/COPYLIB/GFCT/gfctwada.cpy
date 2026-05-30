      ******************************************************************
      * NOME DA INC - GFCTWADA - GFCT5546                              *
      * DESCRICAO   - TARIFAS - EXTRATO ANUAL DE TARIFAS               *
      *               COMBO DE DATA/HR DE SOLIC PREV EMISS CORREIO EXTR*
      *               COMUNICACAO WEB X IMS - ENTRADA                  *
      * TAMANHO     - 0100 (GERAL)                                     *
      *             - 0064 (FIXO)                                      *
      * DATA        - 10/2008                                          *
      * RESPONSAVEL - VALERIA TORQUATO - SONDA PROCWORK - 4506-2349    *
      ******************************************************************
      *
       01  GFCTWADA-ENTRADA.
           03  GFCTWADA-LL                       PIC S9(004) COMP.
           03  GFCTWADA-ZZ                       PIC S9(004) COMP.
           03  GFCTWADA-TRANSACAO                PIC  X(009).
           03  GFCTWADA-FUNCAO                   PIC  X(008).
           03  GFCTWADA-DADOS-RST.
               05  GFCTWADA-QTDE-OCOR            PIC  9(003).
               05  GFCTWADA-QTDE-TOT-REG         PIC  9(006).
               05  GFCTWADA-FUNC-BDSCO           PIC  X(007).
               05  GFCTWADA-FILTRO.
                   07  GFCTWADA-FTR-ANO          PIC  9(004).
                   07  GFCTWADA-FTR-ACAO         PIC  X(001).
      *    'H' - CONSULTA     = HISTORICO
      *    'V' - CANCELAMENTO = VIGENTE
               05  GFCTWADA-PONTEIRO.
                   07  GFCTWADA-PTR-TIMEST       PIC  X(026).
               05  GFCTWADA-FIM                  PIC  X(001).
           03  FILLER                            PIC  X(031).
