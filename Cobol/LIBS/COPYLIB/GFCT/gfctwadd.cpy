      ******************************************************************
      * NOME DA INC - GFCTWADD - GFCT0544                              *
      * DESCRICAO   - TARIFAS - EXTRATO ANUAL DE TARIFAS               *
      *               CONSUL SOLICITACAO PREVIA EMISS EXTRATO CORREIO  *
      *               COMUNICACAO WEB X IMS - SAIDA                    *
      * TAMANHO     - 1010 (GERAL)                                     *
      * DATA        - 10/2008                                          *
      * RESPONSAVEL - VALERIA TORQUATO - SONDA PROCWORK - 4506-2349    *
      ******************************************************************
      *
       01  GFCTWADD-SAIDA.
           03  GFCTWADD-LL                       PIC S9(004) COMP.
           03  GFCTWADD-ZZ                       PIC S9(004) COMP.
           03  GFCTWADD-TRANSACAO                PIC  X(009).
           03  GFCTWADD-FUNCAO                   PIC  X(008).
           03  GFCTWADD-QTDE-OCOR                PIC  9(003).
           03  GFCTWADD-QTDE-TOT-REG             PIC  9(006).
           03  GFCTWADD-FUNC-BDSCO               PIC  X(007).
           03  GFCTWADD-FILTRO.
               05  GFCTWADD-FTR-ANO-SOLIC        PIC  9(004).
               05  GFCTWADD-FTR-TIMEST-SOLIC     PIC  X(026).
               05  GFCTWADD-FTR-ACAO             PIC  X(001).
      *    'H' - CONSULTA     = HISTORICO
           03  GFCTWADD-ERRO                     PIC  9(001).
           03  GFCTWADD-COD-MSG-ERRO             PIC  9(007).
           03  GFCTWADD-COD-SQL-ERRO             PIC  9(003).
           03  GFCTWADD-DESC-MSG-ERRO            PIC  X(079).
           03  GFCTWADD-FIM                      PIC  X(001).
           03  GFCTWADD-DADOS.
               05  GFCTWADD-COD-SITUACAO         PIC  X(001).
      *    A = AGENDADA
      *    P = PROCESSADA
      *    C = CANCELADA
               05  GFCTWADD-QTDE-EXTR-GER        PIC  9(012).
               05  GFCTWADD-DATA-EXECUCAO        PIC  X(010).
               05  GFCTWADD-HORA-EXECUCAO        PIC  X(005).
               05  GFCTWADD-COD-MES-SOLIC        PIC  9(002).
               05  GFCTWADD-COD-AGENC-SOLIC      PIC  9(005).
               05  GFCTWADD-NOMEAGENC-SOLC       PIC  X(040).
               05  GFCTWADD-CODFUNC-INCL         PIC  X(007).
               05  GFCTWADD-NOMEFUNC-INCL        PIC  X(040).
               05  GFCTWADD-DATA-INCLUSAO        PIC  X(010).
               05  GFCTWADD-HORA-INCLUSAO        PIC  X(005).
           03  FILLER                            PIC  X(012).
           03  GFCTWADD-DADOS                    OCCURS 09.
               05  GFCTWADD-COD-AGRUPAMENTO      PIC  9(003).
               05  GFCTWADD-NOME-AGRUPAMENTO     PIC  X(020).
               05  GFCTWADD-COD-SEGMENTO         PIC  9(003).
               05  GFCTWADD-NOME-SEGMENTO        PIC  X(020).
               05  GFCTWADD-VLR-RENTABILID       PIC  9(012)V99.
               05  GFCTWADD-VLR-RENDA            PIC  9(012)V99.
               05  GFCTWADD-CLI-GERENCIADO       PIC  X(001).
               05  GFCTWADD-QTD-TARIFA-DEB       PIC  9(003).
