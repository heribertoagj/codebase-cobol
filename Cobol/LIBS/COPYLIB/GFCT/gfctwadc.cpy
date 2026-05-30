      ******************************************************************
      * NOME DA INC - GFCTWADC - GFCT0544                              *
      * DESCRICAO   - TARIFAS - EXTRATO ANUAL DE TARIFAS               *
      *               CONSUL SOLICITACAO PREVIA EMISS EXTRATO CORREIO  *
      *               COMUNICACAO WEB X IMS - ENTRADA                  *
      * TAMANHO     - 0100 (GERAL)                                     *
      * DATA        - 10/2008                                          *
      * RESPONSAVEL - VALERIA TORQUATO - SONDA PROCWORK - 4506-2349    *
      ******************************************************************
      *
       01  GFCTWADC-ENTRADA.
           03  GFCTWADC-LL                       PIC S9(004) COMP.
           03  GFCTWADC-ZZ                       PIC S9(004) COMP.
           03  GFCTWADC-TRANSACAO                PIC  X(009).
           03  GFCTWADC-FUNCAO                   PIC  X(008).
           03  GFCTWADC-DADOS-RST.
               05  GFCTWADC-QTDE-OCOR            PIC  9(003).
               05  GFCTWADC-QTDE-TOT-REG         PIC  9(006).
               05  GFCTWADC-FUNC-BDSCO           PIC  X(007).
               05  GFCTWADC-FILTRO.
                   07  GFCTWADC-FTR-ANO-SOLIC    PIC  9(004).
                   07  GFCTWADC-FTR-TIMEST-SOLIC PIC  X(026).
                   07  GFCTWADC-FTR-ACAO         PIC  X(001).
      *    'H' - CONSULTA     = HISTORICO
               05  GFCTWADC-FIM                  PIC  X(001).
           03  FILLER                            PIC  X(031).
