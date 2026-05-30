      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. GFCT0212.
       AUTHOR.     JOSSIANE CARLA GAZZONI.
      *================================================================*
      *                   C P M  -  S I S T E M A S                    *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   GFCT0212                                    *
      *    PROGRAMADORA:   JOSSIANE CARLA GAZZONI  - CPM PATO BRANCO   *
      *    ANALISTA CPM:   KHARUZO I. LEITE        - CPM PATO BRANCO   *
      *    ANALISTA....:   MAURICIO                - PROCWORK / GP.50  *
      *    DATA........:   15/06/2005                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   CADASTRO DE TARIFAS - ALTERACAO (PAGINA 4   *
      *      DE 5).                                                    *
      *----------------------------------------------------------------*
      *    BCO DE DADOS:                                               *
      *                DB2                                             *
      *                TABLE                           INCLUDE/BOOK    *
      *                    DB2PRD.HIST_COMPS_PCOTE       GFCTB084      *
      *                    DB2PRD.HIST_EVNTO_CTBIL       GFCTB085      *
      *                    DB2PRD.HIST_TARIF_PRINC       GFCTB087      *
CREMM *                    DB2PRD.TARIF_EVNTO_CTBIL      GFCTB047      *
CREMM *                    DB2PRD.PARM_DATA_PROCM        GFCTB0A1      *
CREMM *                    DB2PRD.PARM_SERVC_TARIF       GFCTB0A2      *
CREMM *                    DB2PRD.SERVC_TARIF_PRINC      GFCTB0D8      *
      *                    DB2PRD.OPERACAO_CTBIL         TRICB003      *
      *                    DB2PRD.DEPDC_BDSCO_ONLINE     MESUB010      *
      *----------------------------------------------------------------*
      *    INC'S.......:                                               *
      *    I#GFCTAX - AREA DE COMUNICACAO - ENTRADA.                   *
      *    I#GFCTAY - AREA DE COMUNICACAO - SAIDA.                     *
      *    I#GFCT0M - AREA DE COMUNICACAO - ERROS.                     *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    GFCT5000 - OBTER DESCRICAO DE MENSAGENS.                    *
      *    POOL0081 - ALOCAR DINAMICAMENTE UM MODULO.                  *
CREMM *================================================================*
CREMM *                                                                *
CREMM *    ALTERACAO:                                                  *
CREMM *    MARCELO CREMM - PROCWORK  -  MARCO-2007                     *
CREMM *                                                                *
CREMM *    TRATAMENTO PARA ALTERACAO DE TARIFA FUTURA. DEVERA CONTEM-  *
CREMM *    PLAR TAMBEM NA TABELA QUENTE.                               *
CREMM *                                                                *
SP0704*----------------------------------------------------------------*
SP0704*                 U L T I M A   A L T E R A C A O                *
SP0704*----------------------------------------------------------------*
SP0704*        SONDA PROCWORK - CONSULTORIA - ALTERACAO - SP0704       *
SP0704*----------------------------------------------------------------*
SP0704*                                                                *
SP0704*    ANALISTA....:  RODRIGO RIOS - SONDA/PROCWORK                *
SP0704*    DATA........:  07/04/2008                                   *
SP0704*    OBJETIVO....:  TRATAR O CANCELAMENTO DAS VERSOES ANTERIORES *
SP0704*                   DE HISTORICOS DA TARIFA, QUANDO FOR CONCUIDA *
SP0704*                   A ALTERACAO DA MESMA.                        *
SP0704*                                                                *
SP1504*----------------------------------------------------------------*
SP1504*                 U L T I M A   A L T E R A C A O                *
SP1504*----------------------------------------------------------------*
SP1504*        SONDA PROCWORK - CONSULTORIA - ALTERACAO - SP1504       *
SP1504*----------------------------------------------------------------*
SP1504*                                                                *
SP1504*    ANALISTA....:  MARCUS VINICIUS - SONDA/PROCWORK             *
SP1504*    DATA........:  15/04/2008                                   *
SP1504*    OBJETIVO....:  TRATAR COBRANCA E ESTORNO PARA TIPO DE       *
SP1504*                   PESSOA JURIDICA E AMBAS (FISICA E JURIDICA)  *
SP1504*                                                                *
161110*================================================================*
161110*                      A L T E R A C A O                         *
161110*----------------------------------------------------------------*
161110*    RESPONSAVEL .: SONDAPROCWORK                                *
161110*    DATA ........: 16/11/2010                                   *
161110*    OBJETIVO ....: INCLUSAO DO TRATAMENTO DO INDICADOR DE       *
161110*                   ISENCAO DE UTILIZACAO DO COMPONENTE TIPO     *
161110*                   TARIFA                                       *
161110*                                                                *
010211*================================================================*
010211*                      A L T E R A C A O                         *
010211*----------------------------------------------------------------*
010211*    RESPONSAVEL .: SONDAPROCWORK                                *
010211*    DATA ........: 01/02/2011                                   *
010211*    OBJETIVO ....: AJUSTE NO TRATAMENTO DO INDICADOR DE ISENCAO *
010211*                   DE UTILIZACAO DO COMPONENTE TIPO TARIFA      *
010211*                                                                *
      *================================================================*
BI0411*                 U L T I M A   A L T E R A C A O                *
BI0411*----------------------------------------------------------------*
BI0411*        SONDA PROCWORK - CONSULTORIA - ALTERACAO - BI0411       *
BI0411*----------------------------------------------------------------*
BI0411*                                                                *
BI0411*    ANALISTA....:  UBIRAJARA(BIRA) - SONDA/PROCWORK             *
BI0411*    DATA........:  04 / 2011                                    *
BI0411*    OBJETIVO....:  TRATAR O CAMPO NOVO CLCTO-ESTRN-ANTER DE     *
BI0411*                   ESTORNO DE TARIFAS SEMESTRE ANTERIOR.        *
BI0411*                                                                *
      *================================================================*
       ENVIRONMENT                     DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       CONFIGURATION                   SECTION.
      *----------------------------------------------------------------*

       SPECIAL-NAMES.
           DECIMAL-POINT               IS COMMA.

           EJECT
      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** GFCT0212 - INICIO DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA INDEXADOR ***'.
      *----------------------------------------------------------------*

       77  IND-1                       PIC  9(001)         VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREAS AUXILIARES ***'.
      *----------------------------------------------------------------*

       77  WRK-SEQANT                  PIC  9(002)         VALUE ZEROS.
       77  WRK-LANCAMENTO              PIC S9(002)         VALUE ZEROS.
       77  WRK-PATRIC                  PIC S9(003)V9(003)  VALUE ZEROS.
       77  WRK-MODULO                  PIC  X(008)         VALUE SPACES.
       77  W-TEM-FUTURA                PIC  X(001)         VALUE 'N'.
       77  WRK-FIM-CSR                 PIC  X(001)         VALUE SPACES.
       77  WRK-FIM-CSR1                PIC  X(001)         VALUE SPACES.

BI0411 01  WRK-S9-3                    PIC +9(009)        VALUE ZEROS.
BI0411 01  FILLER                      REDEFINES          WRK-S9-3.
BI0411     05  FILLER                  PIC  X(007).
BI0411     05  WRK-3-9                 PIC  9(003).

       01  WRK-DATA-DB2.
           03  WRK-DIA-DB2             PIC  9(002)         VALUE ZEROS.
           03  FILLER                  PIC  X(001)         VALUE SPACE.
           03  WRK-MES-DB2             PIC  9(002)         VALUE ZEROS.
           03  FILLER                  PIC  X(001)         VALUE SPACE.
           03  WRK-ANO-DB2             PIC  9(004)         VALUE ZEROS.

       01  WRK-DATA-INI-VIG-TARIF      PIC  9(008)         VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-DATA-INI-VIG-TARIF.
           03  WRK-ANO-INI-VIG-TARIF   PIC  9(004).
           03  WRK-MES-INI-VIG-TARIF   PIC  9(002).
           03  WRK-DIA-INI-VIG-TARIF   PIC  9(002).

       01  WRK-DATA-INI-VIG-COMPS      PIC  9(008)         VALUE ZEROS.
       01  FILLER                      REDEFINES WRK-DATA-INI-VIG-COMPS.
           03  WRK-ANO-INI-VIG-COMPS   PIC  9(004).
           03  WRK-MES-INI-VIG-COMPS   PIC  9(002).
           03  WRK-DIA-INI-VIG-COMPS   PIC  9(002).

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA CAMPOS DO MODULO GFCT5000 ***'.
      *----------------------------------------------------------------*

       01  WRK-5000-AREA.
           03  WRK-5000-AREA-ENVIA.
               05  WRK-5000-AMBIENTE     PIC X(001)        VALUE SPACES.
               05  WRK-5000-COD-MSG      PIC 9(004)        VALUE ZEROS.
           03  WRK-5000-AREA-RECEBE.
               05  WRK-5000-COD-RETORNO  PIC 9(002)        VALUE ZEROS.
               05  WRK-5000-COD-SQL-ERRO PIC 9(003)        VALUE ZEROS.
               05  WRK-5000-DESC-MSG     PIC X(070)        VALUE SPACES.

           EJECT
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE DB2 ***'.
      *----------------------------------------------------------------*

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

           EJECT
           EXEC SQL
               INCLUDE GFCTB084
           END-EXEC.

           EJECT
           EXEC SQL
               INCLUDE GFCTB085
           END-EXEC.

           EJECT
           EXEC SQL
               INCLUDE GFCTB087
           END-EXEC.

           EJECT
           EXEC SQL
               INCLUDE TRICB003
           END-EXEC.

           EJECT
           EXEC SQL
               INCLUDE MESUB010
           END-EXEC.

CREMM      EJECT
CREMM      EXEC SQL
CREMM          INCLUDE GFCTB0A1
CREMM      END-EXEC.
CREMM
CREMM      EJECT
CREMM      EXEC SQL
CREMM          INCLUDE GFCTB0A2
CREMM      END-EXEC.
CREMM
CREMM      EJECT
CREMM      EXEC SQL
CREMM          INCLUDE GFCTB0D8
CREMM      END-EXEC.
CREMM
CREMM      EJECT
CREMM      EXEC SQL
CREMM          INCLUDE GFCTB047
CREMM      END-EXEC.
CREMM
           EJECT
           EXEC SQL
             DECLARE CSR-GFCTB084 CURSOR FOR
               SELECT CSERVC_TARIF,
                      HIDTFD_MANUT_SERVC,
                      CSEQ_MANUT_SERVC,
                      CSEQ_COMPO_PCOTE,
                      DINIC_VGCIA_COMPS,
                      DFIM_VGCIA_COMPS,
                      CTARIF_COMPO_PCOTE,
                      CGRP_SERVC,
                      QFRANQ_COMPO_PCOTE,
                      CPER_COBR_COMPO,
                      VEXCED_COMPO_PCOTE,
                      PEXCED_COMPO_PCOTE,
                      QDIA_COBR_COMPO,
                      RSEGDA_LIN_EXCED,
161110                DDIA_COBR_TARIF,
161110                CIDTFD_COBR_FERI,
161110                CIDTFD_VLR_EXCED,
161110                CIDTFD_CTRL_UTILZ,
161110                CINDCD_ISENC_UTILZ
             FROM     DB2PRD.HIST_COMPS_PCOTE
             WHERE    CSERVC_TARIF       = :GFCTB084.CSERVC-TARIF
               AND    CSEQ_MANUT_SERVC   = :GFCTB084.CSEQ-MANUT-SERVC
               AND    DFIM_VGCIA_COMPS  >= :GFCTB084.DFIM-VGCIA-COMPS
             ORDER BY CSEQ_COMPO_PCOTE
           END-EXEC.

           EJECT
           EXEC SQL
             DECLARE CSR-GFCTB085 CURSOR FOR
             SELECT  CSERVC_TARIF,
                     HIDTFD_MANUT_SERVC,
                     CSEQ_MANUT_SERVC,
                     CTPO_LCTO_CTBIL,
                     CSEQ_EVNTO_CTBIL,
                     CINDCD_DEPDC_CTBIL,
                     PATRIC_CTBIL,
                     CDEPDC_CTLZO,
                     CPRODT_CTBIL,
                     COPER_CTBIL
             FROM    DB2PRD.HIST_EVNTO_CTBIL
             WHERE   CSERVC_TARIF      = :GFCTB085.CSERVC-TARIF
               AND   CSEQ_MANUT_SERVC  = :GFCTB085.CSEQ-MANUT-SERVC
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** GFCT0212 - FIM DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       COPY I#GFCTAX.

           EJECT
       COPY I#GFCTAY.

           EJECT
       COPY I#GFCT0M.

           EJECT
      *================================================================*
       PROCEDURE DIVISION              USING GFCTAX-ENTRADA
                                             GFCTAY-SAIDA
                                             GFCT0M-AREA-ERROS.
      *================================================================*

      *----------------------------------------------------------------*
       0000-00-INICIAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 1000-00-INICIALIZAR.

           PERFORM 2000-00-PROCESSAR.

           PERFORM 4000-00-FINALIZAR.

      *----------------------------------------------------------------*
       0000-00-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1000-00-INICIALIZAR              SECTION.
      *----------------------------------------------------------------*

           MOVE ZEROS                  TO WRK-PATRIC.
           MOVE SPACES                 TO GFCT0M-AREA-ERROS
                                          GFCTAY-SAIDA.

           INITIALIZE GFCT0M-AREA-ERROS
                      GFCTAY-SAIDA.

           PERFORM 1100-00-MONTAR-AREA-SAIDA.

           PERFORM 1200-00-CONSISTIR-DADOS.

      *----------------------------------------------------------------*
       1000-00-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       1100-00-MONTAR-AREA-SAIDA        SECTION.
      *----------------------------------------------------------------*

           MOVE +1010                TO GFCTAY-LL.
           MOVE GFCTAX-ZZ            TO GFCTAY-ZZ.
           MOVE GFCTAX-TRANSACAO     TO GFCTAY-TRANSACAO.
           MOVE GFCTAX-FUNCAO        TO GFCTAY-FUNCAO.
           MOVE GFCTAX-CSERVC-TARIF  TO GFCTAY-CSERVC-TARIF.
           MOVE GFCTAX-CTPO-SERVC-TARIF
                                     TO GFCTAY-CTPO-SERVC-TARIF.
           MOVE GFCTAX-DINIC-VGCIA-TARIF
                                     TO GFCTAY-DINIC-VGCIA-TARIF.
           MOVE GFCTAX-DFIM-VGCIA-TARIF
                                     TO GFCTAY-DFIM-VGCIA-TARIF.
           MOVE GFCTAX-RSERVC-TARIF-REDZD
                                     TO GFCTAY-RSERVC-TARIF-REDZD.
           MOVE GFCTAX-HIDTFD-MANUT-SERVC
                                     TO GFCTAY-HIDTFD-MANUT-SERVC.
           MOVE GFCTAX-CSEQ-MANUT-SERVC
                                     TO GFCTAY-CSEQ-MANUT-SERVC.
           MOVE GFCTAX-CPRODT-CTBIL-ELEMT
                                     TO GFCTAY-CPRODT-CTBIL-ELEMT.
SP1504     MOVE GFCTAX-CPSSOA-SERVC-TARIF
SP1504                               TO GFCTAY-CPSSOA-SERVC-TARIF.
           MOVE GFCTAX-CTPO-LCTO-COBR
                                     TO GFCTAY-CTPO-LCTO-COBR.
           MOVE GFCTAX-CTPO-LCTO-ESTOR
                                     TO GFCTAY-CTPO-LCTO-ESTOR.
SP1504     MOVE GFCTAX-CTPO-LCTO-COBR-PJ
SP1504                               TO GFCTAY-CTPO-LCTO-COBR-PJ.
SP1504     MOVE GFCTAX-CTPO-LCTO-ESTOR-PJ
SP1504                               TO GFCTAY-CTPO-LCTO-ESTOR-PJ.
BI0411     MOVE GFCTAX-CLCTO-ESTRN-ANTER
BI0411                               TO GFCTAY-CLCTO-ESTRN-ANTER.
           MOVE GFCTAX-ACAO          TO GFCTAY-ACAO.
           MOVE GFCTAX-ACAO-TELA     TO GFCTAY-ACAO-TELA.
           MOVE GFCTAX-MASTER        TO GFCTAY-MASTER.
           MOVE GFCTAX-FUNC-BDSCO    TO GFCTAY-FUNC-BDSCO.
           MOVE GFCTAX-TERMINAL      TO GFCTAY-TERMINAL.
           MOVE GFCTAX-AGEN-DEPTO    TO GFCTAY-AGEN-DEPTO.
           MOVE GFCTAX-FIM           TO GFCTAY-FIM.

           MOVE ZEROS                TO WRK-5000-COD-MSG.

           PERFORM 1110-00-OBTER-DESCRICAO-MSG.

           MOVE WRK-5000-DESC-MSG    TO GFCTAY-DESC-MSG-ERRO.
           MOVE 'S'                  TO GFCTAY-FIM.

      *----------------------------------------------------------------*
       1100-00-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1110-00-OBTER-DESCRICAO-MSG      SECTION.
      *----------------------------------------------------------------*

           MOVE 'O'                          TO WRK-5000-AMBIENTE.
           MOVE 'GFCT5000'                   TO WRK-MODULO.

           CALL 'POOL0081'                   USING WRK-MODULO
                                                   WRK-5000-AREA-ENVIA
                                                   WRK-5000-AREA-RECEBE.

           IF  WRK-5000-COD-RETORNO           NOT EQUAL ZEROS
               IF  WRK-5000-COD-RETORNO       EQUAL 01
                   MOVE 1                     TO GFCTAY-ERRO
                   MOVE ZEROS                 TO GFCTAY-COD-SQL-ERRO
                                                 GFCTAY-COD-MSG-ERRO
                   MOVE WRK-5000-DESC-MSG     TO GFCTAY-DESC-MSG-ERRO
               ELSE
                   IF  WRK-5000-COD-RETORNO   EQUAL 99
                       MOVE 1                 TO GFCTAY-ERRO
                       MOVE WRK-5000-COD-SQL-ERRO
                                              TO GFCTAY-COD-SQL-ERRO
                       MOVE ZEROS             TO GFCTAY-COD-MSG-ERRO
                       MOVE WRK-5000-DESC-MSG TO GFCTAY-DESC-MSG-ERRO
                   END-IF
               END-IF

               INITIALIZE GFCTAY-DADOS

               PERFORM 4000-00-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       1110-00-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       1200-00-CONSISTIR-DADOS          SECTION.
      *----------------------------------------------------------------*

           IF  GFCTAX-TRANSACAO            EQUAL LOW-VALUES  OR
               GFCTAX-TRANSACAO            EQUAL SPACES      OR
               GFCTAX-FUNCAO               EQUAL LOW-VALUES  OR
               GFCTAX-FUNCAO               EQUAL SPACES      OR
               GFCTAX-MASTER               EQUAL LOW-VALUES  OR
               GFCTAX-MASTER               EQUAL SPACES      OR
               GFCTAX-FUNC-BDSCO           EQUAL LOW-VALUES  OR
               GFCTAX-FUNC-BDSCO           EQUAL SPACES      OR
               GFCTAX-DINIC-VGCIA-TARIF    EQUAL LOW-VALUES  OR
               GFCTAX-DINIC-VGCIA-TARIF    EQUAL SPACES      OR
               GFCTAX-DFIM-VGCIA-TARIF     EQUAL LOW-VALUES  OR
               GFCTAX-DFIM-VGCIA-TARIF     EQUAL SPACES      OR
               GFCTAX-HIDTFD-MANUT-SERVC   EQUAL LOW-VALUES  OR
               GFCTAX-HIDTFD-MANUT-SERVC   EQUAL SPACES      OR
               GFCTAX-RSERVC-TARIF-REDZD   EQUAL LOW-VALUES  OR
               GFCTAX-RSERVC-TARIF-REDZD   EQUAL SPACES      OR
               GFCTAX-CPRODT-CTBIL-ELEMT   EQUAL LOW-VALUES  OR
               GFCTAX-CPRODT-CTBIL-ELEMT   EQUAL SPACES      OR
               GFCTAX-CSERVC-TARIF         NOT NUMERIC       OR
               GFCTAX-CTPO-SERVC-TARIF     NOT NUMERIC       OR
               GFCTAX-CSEQ-MANUT-SERVC     NOT NUMERIC       OR
SP1504        (GFCTAX-CPSSOA-SERVC-TARIF   NOT EQUAL 'F' AND 'J'
SP1504                                           AND 'A')    OR
               GFCTAX-CTPO-LCTO-COBR       NOT NUMERIC       OR
               GFCTAX-CTPO-LCTO-ESTOR      NOT NUMERIC       OR
SP1504         GFCTAX-CTPO-LCTO-COBR-PJ    NOT NUMERIC       OR
SP1504         GFCTAX-CTPO-LCTO-ESTOR-PJ   NOT NUMERIC       OR
BI0411         GFCTAX-CLCTO-ESTRN-ANTER    NOT NUMERIC       OR
               GFCTAX-AGEN-DEPTO           NOT NUMERIC       OR
              (GFCTAX-FIM                  NOT EQUAL 'S' AND 'N')
               MOVE 1                      TO GFCTAY-ERRO
                                              GFCTAY-COD-MSG-ERRO
                                              WRK-5000-COD-MSG
               MOVE ZEROS                  TO GFCTAY-COD-SQL-ERRO

               PERFORM 1110-00-OBTER-DESCRICAO-MSG

               MOVE WRK-5000-DESC-MSG      TO GFCTAY-DESC-MSG-ERRO

               PERFORM 4000-00-FINALIZAR
           END-IF.

CREMM      MOVE 'GFCT'                 TO CSIST-PRINC      OF GFCTB0A1.
CREMM
CREMM      EXEC SQL
CREMM        SELECT
CREMM              CSIST_PRINC,
CREMM              DPROCM_ANTER,
CREMM              DPROCM_ATUAL,
CREMM              DPROCM_PROX
CREMM        INTO
CREMM              :GFCTB0A1.CSIST-PRINC,
CREMM              :GFCTB0A1.DPROCM-ANTER,
CREMM              :GFCTB0A1.DPROCM-ATUAL,
CREMM              :GFCTB0A1.DPROCM-PROX
CREMM        FROM   DB2PRD.PARM_DATA_PROCM
CREMM        WHERE
CREMM              CSIST_PRINC        = :GFCTB0A1.CSIST-PRINC
CREMM      END-EXEC.
CREMM
CREMM      IF (SQLCODE                NOT EQUAL ZEROS) OR
CREMM         (SQLWARN0               EQUAL 'W')
CREMM          INITIALIZE             GFCT0M-ERRO-SQL
CREMM
CREMM          MOVE 'GFCT0212'        TO GFCT0M-PROGRAMA
CREMM          MOVE 'DB2'             TO GFCT0M-TIPO-ACESSO
CREMM          MOVE 'PARM_DATA_PROCM'
CREMM                                 TO GFCT0M-NOME-TAB
CREMM          MOVE 'SELECT'          TO GFCT0M-COMANDO-SQL
CREMM          MOVE  SQLCODE          TO GFCT0M-SQLCODE
CREMM                                    GFCTAY-COD-SQL-ERRO
CREMM          MOVE '0010'            TO GFCT0M-LOCAL
CREMM          MOVE  SQLCA            TO GFCT0M-SQLCA-AREA
CREMM          MOVE  9                TO GFCTAY-ERRO
CREMM          MOVE  12               TO GFCTAY-COD-MSG-ERRO
CREMM                                    WRK-5000-COD-MSG
CREMM
CREMM          PERFORM 1110-00-OBTER-DESCRICAO-MSG
CREMM
CREMM          STRING WRK-5000-DESC-MSG
CREMM                 ' - GFCTB0A1'
CREMM          DELIMITED BY '  '      INTO GFCTAY-DESC-MSG-ERRO
CREMM
CREMM          INITIALIZE GFCTAY-DADOS
CREMM
CREMM          PERFORM 4000-00-FINALIZAR
CREMM      END-IF.
CREMM
CREMM      MOVE GFCTAX-CSERVC-TARIF   TO CSERVC-TARIF
CREMM                                 OF GFCTB0A2.
CREMM      MOVE DPROCM-ATUAL          TO DINIC-VGCIA-TARIF
CREMM                                 OF GFCTB0A2.
CREMM      EXEC SQL
CREMM        SELECT
CREMM              CSERVC_TARIF,
CREMM              DINIC_VGCIA_TARIF
CREMM        INTO
CREMM              :GFCTB0A2.CSERVC-TARIF,
CREMM              :GFCTB0A2.DINIC-VGCIA-TARIF
CREMM        FROM   DB2PRD.PARM_SERVC_TARIF
CREMM        WHERE
CREMM              CSERVC_TARIF       = :GFCTB0A2.CSERVC-TARIF
CREMM          AND DINIC_VGCIA_TARIF  > :GFCTB0A2.DINIC-VGCIA-TARIF
CREMM      END-EXEC.
CREMM
CREMM      IF (SQLCODE                NOT EQUAL ZEROS AND +100) OR
CREMM         (SQLWARN0               EQUAL 'W')
CREMM          INITIALIZE             GFCT0M-ERRO-SQL
CREMM
CREMM          MOVE 'GFCT0212'        TO GFCT0M-PROGRAMA
CREMM          MOVE 'DB2'             TO GFCT0M-TIPO-ACESSO
CREMM          MOVE 'PARM_SERVC_TARIF'
CREMM                                 TO GFCT0M-NOME-TAB
CREMM          MOVE 'SELECT'          TO GFCT0M-COMANDO-SQL
CREMM          MOVE  SQLCODE          TO GFCT0M-SQLCODE
CREMM                                    GFCTAY-COD-SQL-ERRO
CREMM          MOVE '0020'            TO GFCT0M-LOCAL
CREMM          MOVE  SQLCA            TO GFCT0M-SQLCA-AREA
CREMM          MOVE  9                TO GFCTAY-ERRO
CREMM          MOVE  12               TO GFCTAY-COD-MSG-ERRO
CREMM                                    WRK-5000-COD-MSG
CREMM
CREMM          PERFORM 1110-00-OBTER-DESCRICAO-MSG
CREMM
CREMM          STRING WRK-5000-DESC-MSG
CREMM                 ' - GFCTB0A2'
CREMM          DELIMITED BY '  '      INTO GFCTAY-DESC-MSG-ERRO
CREMM
CREMM          INITIALIZE GFCTAY-DADOS
CREMM
CREMM          PERFORM 4000-00-FINALIZAR
CREMM      END-IF.
CREMM
CREMM      IF SQLCODE EQUAL ZEROS
CREMM         MOVE 'S' TO W-TEM-FUTURA
CREMM      END-IF.

      *----------------------------------------------------------------*
       1200-00-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2000-00-PROCESSAR                SECTION.
      *----------------------------------------------------------------*

SP1504     IF  GFCTAX-CPSSOA-SERVC-TARIF
SP1504                                 EQUAL 'F'
SP1504         IF  GFCTAX-CTPO-LCTO-COBR
SP1504                                 EQUAL ZEROS AND
SP1504             GFCTAX-CTPO-LCTO-ESTOR
SP1504                                 EQUAL ZEROS
SP1504             MOVE 1              TO GFCTAY-ERRO
SP1504             MOVE ZEROS          TO GFCTAY-COD-SQL-ERRO
SP1504             MOVE 1425           TO GFCTAY-COD-MSG-ERRO
SP1504                                    WRK-5000-COD-MSG
SP1504
SP1504             PERFORM 1110-00-OBTER-DESCRICAO-MSG
SP1504
SP1504             MOVE WRK-5000-DESC-MSG
SP1504                                 TO GFCTAY-DESC-MSG-ERRO
SP1504
SP1504             INITIALIZE GFCTAY-DADOS
SP1504
SP1504             PERFORM 4000-00-FINALIZAR
SP1504         END-IF
SP1504     ELSE
SP1504         IF  GFCTAX-CPSSOA-SERVC-TARIF
SP1504                                 EQUAL 'J'
SP1504             IF  GFCTAX-CTPO-LCTO-COBR-PJ
SP1504                                 EQUAL ZEROS AND
SP1504                 GFCTAX-CTPO-LCTO-ESTOR-PJ
SP1504                                 EQUAL ZEROS
SP1504                 MOVE 1          TO GFCTAY-ERRO
SP1504                 MOVE ZEROS      TO GFCTAY-COD-SQL-ERRO
SP1504                 MOVE 1425       TO GFCTAY-COD-MSG-ERRO
SP1504                                    WRK-5000-COD-MSG
SP1504
SP1504                 PERFORM 1110-00-OBTER-DESCRICAO-MSG
SP1504
SP1504                 MOVE WRK-5000-DESC-MSG
SP1504                                 TO GFCTAY-DESC-MSG-ERRO
SP1504
SP1504                 INITIALIZE GFCTAY-DADOS
SP1504
SP1504                 PERFORM 4000-00-FINALIZAR
SP1504             END-IF
SP1504         ELSE
SP1504             IF  GFCTAX-CTPO-LCTO-COBR
SP1504                                 EQUAL ZEROS AND
SP1504                 GFCTAX-CTPO-LCTO-ESTOR
SP1504                                 EQUAL ZEROS AND
SP1504                 GFCTAX-CTPO-LCTO-COBR-PJ
SP1504                                 EQUAL ZEROS AND
SP1504                 GFCTAX-CTPO-LCTO-ESTOR-PJ
SP1504                                 EQUAL ZEROS
SP1504                 MOVE 1          TO GFCTAY-ERRO
SP1504                 MOVE ZEROS      TO GFCTAY-COD-SQL-ERRO
SP1504                 MOVE 1425       TO GFCTAY-COD-MSG-ERRO
SP1504                                    WRK-5000-COD-MSG
SP1504
SP1504                 PERFORM 1110-00-OBTER-DESCRICAO-MSG
SP1504
SP1504                 MOVE WRK-5000-DESC-MSG
SP1504                                 TO GFCTAY-DESC-MSG-ERRO
SP1504
SP1504                 INITIALIZE GFCTAY-DADOS
SP1504
SP1504                 PERFORM 4000-00-FINALIZAR
SP1504             END-IF
SP1504         END-IF
SP1504     END-IF.

SP1504     IF  GFCTAX-CTPO-LCTO-COBR   NOT EQUAL ZEROS OR
SP1504         GFCTAX-CTPO-LCTO-ESTOR  NOT EQUAL ZEROS
SP1504         IF  GFCTAX-CTPO-LCTO-COBR
SP1504                                 EQUAL ZEROS
SP1504             MOVE 1              TO GFCTAY-ERRO
SP1504             MOVE ZEROS          TO GFCTAY-COD-SQL-ERRO
SP1504             MOVE 1421           TO GFCTAY-COD-MSG-ERRO
SP1504                                    WRK-5000-COD-MSG
SP1504
SP1504             PERFORM 1110-00-OBTER-DESCRICAO-MSG
SP1504
SP1504             MOVE WRK-5000-DESC-MSG
SP1504                                 TO GFCTAY-DESC-MSG-ERRO
SP1504
SP1504             INITIALIZE GFCTAY-DADOS
SP1504
SP1504             PERFORM 4000-00-FINALIZAR
SP1504         ELSE
SP1504             IF  GFCTAX-CTPO-LCTO-ESTOR
SP1504                                 EQUAL ZEROS
SP1504                 MOVE 1          TO GFCTAY-ERRO
SP1504                 MOVE ZEROS      TO GFCTAY-COD-SQL-ERRO
SP1504                 MOVE 1422       TO GFCTAY-COD-MSG-ERRO
SP1504                                    WRK-5000-COD-MSG
SP1504
SP1504                 PERFORM 1110-00-OBTER-DESCRICAO-MSG
SP1504
SP1504                 MOVE WRK-5000-DESC-MSG
SP1504                                 TO GFCTAY-DESC-MSG-ERRO
SP1504
SP1504                 INITIALIZE GFCTAY-DADOS
SP1504
SP1504                 PERFORM 4000-00-FINALIZAR
SP1504             ELSE
                       IF  GFCTAX-CTPO-LCTO-COBR
                                       EQUAL GFCTAX-CTPO-LCTO-ESTOR
                           MOVE 1      TO GFCTAY-ERRO
                           MOVE ZEROS  TO GFCTAY-COD-SQL-ERRO
                           MOVE 414    TO GFCTAY-COD-MSG-ERRO
                                          WRK-5000-COD-MSG

                           PERFORM 1110-00-OBTER-DESCRICAO-MSG

                           MOVE WRK-5000-DESC-MSG
                                       TO GFCTAY-DESC-MSG-ERRO

                           INITIALIZE GFCTAY-DADOS

                           PERFORM 4000-00-FINALIZAR
                       END-IF
SP1504             END-IF
SP1504         END-IF
SP1504     END-IF.


SP1504     IF  GFCTAX-CTPO-LCTO-COBR-PJ
SP1504                                 NOT EQUAL ZEROS OR
SP1504         GFCTAX-CTPO-LCTO-ESTOR-PJ
SP1504                                 NOT EQUAL ZEROS
SP1504         IF  GFCTAX-CTPO-LCTO-COBR-PJ
SP1504                                 EQUAL ZEROS
SP1504             MOVE 1              TO GFCTAY-ERRO
SP1504             MOVE ZEROS          TO GFCTAY-COD-SQL-ERRO
SP1504             MOVE 1423           TO GFCTAY-COD-MSG-ERRO
SP1504                                    WRK-5000-COD-MSG
SP1504
SP1504             PERFORM 1110-00-OBTER-DESCRICAO-MSG
SP1504
SP1504             MOVE WRK-5000-DESC-MSG
SP1504                                 TO GFCTAY-DESC-MSG-ERRO
SP1504
SP1504             INITIALIZE GFCTAY-DADOS
SP1504
SP1504             PERFORM 4000-00-FINALIZAR
SP1504         ELSE
SP1504             IF  GFCTAX-CTPO-LCTO-ESTOR-PJ
SP1504                                 EQUAL ZEROS
SP1504                 MOVE 1          TO GFCTAY-ERRO
SP1504                 MOVE ZEROS      TO GFCTAY-COD-SQL-ERRO
SP1504                 MOVE 1424       TO GFCTAY-COD-MSG-ERRO
SP1504                                    WRK-5000-COD-MSG
SP1504
SP1504                 PERFORM 1110-00-OBTER-DESCRICAO-MSG
SP1504
SP1504                 MOVE WRK-5000-DESC-MSG
SP1504                                 TO GFCTAY-DESC-MSG-ERRO
SP1504
SP1504                 INITIALIZE GFCTAY-DADOS
SP1504
SP1504                 PERFORM 4000-00-FINALIZAR
SP1504             ELSE
SP1504                 IF  GFCTAX-CTPO-LCTO-COBR-PJ
SP1504                                 EQUAL GFCTAX-CTPO-LCTO-ESTOR-PJ
SP1504                     MOVE 1      TO GFCTAY-ERRO
SP1504                     MOVE ZEROS  TO GFCTAY-COD-SQL-ERRO
SP1504                     MOVE 1418   TO GFCTAY-COD-MSG-ERRO
SP1504                                    WRK-5000-COD-MSG
SP1504
SP1504                     PERFORM 1110-00-OBTER-DESCRICAO-MSG
SP1504
SP1504                     MOVE WRK-5000-DESC-MSG
SP1504                                 TO GFCTAY-DESC-MSG-ERRO
SP1504
SP1504                     INITIALIZE GFCTAY-DADOS
SP1504
SP1504                     PERFORM 4000-00-FINALIZAR
SP1504                 END-IF
SP1504             END-IF
SP1504         END-IF
SP1504     END-IF.

BI0411     IF  GFCTAX-CLCTO-ESTRN-ANTER  EQUAL ZEROS
BI0411         MOVE 1          TO GFCTAY-ERRO
BI0411         MOVE ZEROS      TO GFCTAY-COD-SQL-ERRO
BI0411         MOVE 1836       TO GFCTAY-COD-MSG-ERRO
BI0411                            WRK-5000-COD-MSG
BI0411         PERFORM 1110-00-OBTER-DESCRICAO-MSG
BI0411         MOVE WRK-5000-DESC-MSG
BI0411                         TO GFCTAY-DESC-MSG-ERRO
BI0411         INITIALIZE GFCTAY-DADOS
BI0411         PERFORM 4000-00-FINALIZAR
BI0411     END-IF.

           IF  GFCTAX-ACAO                 EQUAL 'I'
               IF  GFCTAX-ACAO-TELA        EQUAL 'I' OR 'U' OR 'E'
                   PERFORM 2100-00-INCLUIR-PARAMETRO-CONT
               ELSE
                   IF  GFCTAX-ACAO-TELA    EQUAL 'A' OR 'C'
                       PERFORM 2200-00-ABRIR-CURSOR

                       PERFORM 2300-00-LER-CURSOR

                       PERFORM 2700-00-PROCESSAR-INCLUSAO
                         UNTIL WRK-FIM-CSR EQUAL 'S'

                       PERFORM 2800-00-FECHAR-CURSOR

                       PERFORM 3000-00-ABRIR-CSR-GFCTB084

                       PERFORM 3100-00-LER-CSR-GFCTB084

                       PERFORM 3200-00-PROCESSAR-INCLUSAO
                         UNTIL WRK-FIM-CSR1 EQUAL 'S'

                       PERFORM 3300-00-FECHAR-CSR-GFCTB084

                       PERFORM 2600-00-ALTERAR-HIST-PRINC
                   END-IF
               END-IF
           ELSE
               EVALUATE GFCTAX-ACAO-TELA
                   WHEN 'I'
                        PERFORM 2100-00-INCLUIR-PARAMETRO-CONT
                   WHEN 'U'
                        PERFORM 2900-00-ALTERAR-PARAMETRO-CONT
                   WHEN 'E'
                        PERFORM 2921-00-EXCLUIR-PARAMETRO-CONT
                   WHEN 'A'
SP1504                  IF  GFCTAX-CPSSOA-SERVC-TARIF
SP1504                                 EQUAL 'A' OR 'F'
                            PERFORM 2400-00-VERIFICAR-COBRANCA

                            PERFORM 2500-00-VERIFICAR-ESTORNO
SP1504                  END-IF

SP1504                  IF  GFCTAX-CPSSOA-SERVC-TARIF
SP1504                                 EQUAL 'A' OR 'J'
SP1504                      PERFORM 2420-00-VERIFICAR-COBRANCA-PJ
SP1504
SP1504                      PERFORM 2520-00-VERIFICAR-ESTORNO-PJ
SP1504                  END-IF

BI0411                  PERFORM 2540-00-VERIFICA-ESTORNO-ANTER

                        PERFORM 2600-00-ALTERAR-HIST-PRINC
                   WHEN 'C'
SP1504                  IF  GFCTAX-CPSSOA-SERVC-TARIF
SP1504                                 EQUAL 'A' OR 'F'
                            PERFORM 2400-00-VERIFICAR-COBRANCA

                            PERFORM 2500-00-VERIFICAR-ESTORNO
SP1504                  END-IF

SP1504                  IF  GFCTAX-CPSSOA-SERVC-TARIF
SP1504                                 EQUAL 'A' OR 'J'
SP1504                      PERFORM 2420-00-VERIFICAR-COBRANCA-PJ
SP1504
SP1504                      PERFORM 2520-00-VERIFICAR-ESTORNO-PJ
SP1504                  END-IF

BI0411                  PERFORM 2540-00-VERIFICA-ESTORNO-ANTER

                        PERFORM 2600-00-ALTERAR-HIST-PRINC
               END-EVALUATE
           END-IF.

      *----------------------------------------------------------------*
       2000-00-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       2100-00-INCLUIR-PARAMETRO-CONT   SECTION.
      *----------------------------------------------------------------*

           PERFORM
            VARYING IND-1              FROM 1 BY 1
              UNTIL IND-1              GREATER 5
               IF (GFCTAX-ACAO                EQUAL 'I' AND
                   GFCTAX-ACAO-TELA           EQUAL 'E' AND
                   GFCTAX-OPCAO(IND-1)        NOT EQUAL 'N') OR
                  (GFCTAX-ACAO                EQUAL 'U' AND
                   GFCTAX-ACAO-TELA           EQUAL 'I' AND
                   GFCTAX-OPCAO(IND-1)        NOT EQUAL 'S')
                   NEXT SENTENCE
               ELSE
                   COMPUTE WRK-PATRIC = WRK-PATRIC
                                      + GFCTAX-PATRIC-CTBIL(IND-1)
               END-IF
           END-PERFORM.

           PERFORM 2110-00-VALIDAR-PERCENTUAL.

           PERFORM
            VARYING IND-1              FROM 1 BY 1
              UNTIL IND-1              GREATER 5
               IF (GFCTAX-ACAO                EQUAL 'I' AND
                   GFCTAX-ACAO-TELA           EQUAL 'E' AND
                   GFCTAX-OPCAO(IND-1)        NOT EQUAL 'N') OR
                  (GFCTAX-ACAO                EQUAL 'U' AND
                   GFCTAX-ACAO-TELA           EQUAL 'I' AND
                   GFCTAX-OPCAO(IND-1)        NOT EQUAL 'S')
                   GO TO 2100-00-99-FIM
               ELSE
                   PERFORM 2120-00-EFETUAR-INCLUSAO
               END-IF
           END-PERFORM.

      *----------------------------------------------------------------*
       2100-00-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2110-00-VALIDAR-PERCENTUAL       SECTION.
      *----------------------------------------------------------------*

           IF  WRK-PATRIC              NOT EQUAL 100
               MOVE 1                  TO GFCTAY-ERRO
               MOVE ZEROS              TO GFCTAY-COD-SQL-ERRO
               MOVE 215                TO GFCTAY-COD-MSG-ERRO
                                          WRK-5000-COD-MSG

               PERFORM 1110-00-OBTER-DESCRICAO-MSG

               MOVE WRK-5000-DESC-MSG  TO GFCTAY-DESC-MSG-ERRO

               INITIALIZE GFCTAY-DADOS

               PERFORM 4000-00-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2110-00-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       2120-00-EFETUAR-INCLUSAO         SECTION.
      *----------------------------------------------------------------*

           IF  GFCTAX-CINDCD-DEPDC-CTBIL(IND-1)
                                          EQUAL 3
               IF  GFCTAX-CJUNC-DEPDC-CTRLZ(IND-1)
                                          EQUAL ZEROS
                   MOVE 1                 TO GFCTAY-ERRO
                   MOVE ZEROS             TO GFCTAY-COD-SQL-ERRO
                   MOVE 2                 TO GFCTAY-COD-MSG-ERRO
                                             WRK-5000-COD-MSG

                   PERFORM 1110-00-OBTER-DESCRICAO-MSG

                   MOVE WRK-5000-DESC-MSG TO GFCTAY-DESC-MSG-ERRO

                   INITIALIZE GFCTAY-DADOS

                   PERFORM 4000-00-FINALIZAR
               ELSE
                   PERFORM 2121-00-VALIDAR-DEP-CENTR
               END-IF
           END-IF.

           PERFORM 2122-00-VALIDAR-OPER-TRIC.

           MOVE GFCTAX-CSERVC-TARIF       TO CSERVC-TARIF OF GFCTB085.
           MOVE GFCTAX-HIDTFD-MANUT-SERVC TO HIDTFD-MANUT-SERVC
                                                          OF GFCTB085.
           MOVE GFCTAX-CSEQ-MANUT-SERVC   TO CSEQ-MANUT-SERVC
                                                          OF GFCTB085.
           MOVE ZEROS                     TO CPRODT-CTBIL OF GFCTB085.
           MOVE GFCTAX-COPER-CTBIL(IND-1) TO COPER-CTBIL  OF GFCTB085.
           MOVE GFCTAX-CTPO-LCTO-CTBIL(IND-1)
                                          TO CTPO-LCTO-CTBIL
                                                          OF GFCTB085.
           MOVE GFCTAX-CSEQ-EVNTO-CTBIL(IND-1)
                                          TO CSEQ-EVNTO-CTBIL
                                                          OF GFCTB085.
           MOVE GFCTAX-CINDCD-DEPDC-CTBIL(IND-1)
                                          TO CINDCD-DEPDC-CTBIL
                                                          OF GFCTB085.
           MOVE GFCTAX-PATRIC-CTBIL(IND-1)
                                          TO PATRIC-CTBIL OF GFCTB085.
           MOVE GFCTAX-CJUNC-DEPDC-CTRLZ(IND-1)
                                          TO CDEPDC-CTLZO OF GFCTB085.

           MOVE ZEROS                     TO CEMPR        OF GFCTB085
                                             CGRP-RZ-CTBIL
                                                          OF GFCTB085
                                             CSGRP-RZ-CTBIL
                                                          OF GFCTB085
                                             CCTA-EVNTO-CTBIL
                                                          OF GFCTB085
                                             CDOCTO-CTBIL OF GFCTB085

           PERFORM 2123-00-INSERIR-GFCTB085.

CREMM      IF  GFCTAX-MASTER   EQUAL 'S' AND
CREMM          W-TEM-FUTURA    EQUAL 'S'
CREMM          PERFORM 2124-00-INSERIR-GFCTB047
CREMM      END-IF.

      *----------------------------------------------------------------*
       2120-00-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2121-00-VALIDAR-DEP-CENTR        SECTION.
      *----------------------------------------------------------------*

           MOVE 237                   TO CEMPR-INC OF MESUB010.
           MOVE GFCTAX-CJUNC-DEPDC-CTRLZ(IND-1)
                                      TO CDEPDC    OF MESUB010.

           EXEC SQL
             SELECT CEMPR_INC
             INTO  :MESUB010.CEMPR-INC
             FROM   DB2PRD.DEPDC_BDSCO_ONLINE
             WHERE  CEMPR_INC         = :MESUB010.CEMPR-INC
               AND  CDEPDC            = :MESUB010.CDEPDC
           END-EXEC.

           IF (SQLCODE                NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0               EQUAL 'W')
               INITIALIZE             GFCT0M-ERRO-SQL

               MOVE 'GFCT0212'        TO GFCT0M-PROGRAMA
               MOVE 'DB2'             TO GFCT0M-TIPO-ACESSO
               MOVE 'DEPDC_BDSCO_ONLINE'
                                      TO GFCT0M-NOME-TAB
               MOVE 'SELECT'          TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE          TO GFCT0M-SQLCODE
                                         GFCTAY-COD-SQL-ERRO
               MOVE '0030'            TO GFCT0M-LOCAL
               MOVE  SQLCA            TO GFCT0M-SQLCA-AREA
               MOVE  9                TO GFCTAY-ERRO
               MOVE  12               TO GFCTAY-COD-MSG-ERRO
                                         WRK-5000-COD-MSG

               PERFORM 1110-00-OBTER-DESCRICAO-MSG

               STRING WRK-5000-DESC-MSG
                      ' - MESUB010'
               DELIMITED BY '  '      INTO GFCTAY-DESC-MSG-ERRO

               INITIALIZE GFCTAY-DADOS

               PERFORM 4000-00-FINALIZAR
           END-IF.

           IF  SQLCODE                EQUAL +100
               MOVE 1                 TO GFCTAY-COD-MSG-ERRO-GEN(IND-1)
               MOVE 112               TO WRK-5000-COD-MSG

               PERFORM 1110-00-OBTER-DESCRICAO-MSG

               MOVE WRK-5000-DESC-MSG TO GFCTAY-DESC-MSG-ERRO-GEN(IND-1)

               PERFORM 4000-00-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2121-00-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       2122-00-VALIDAR-OPER-TRIC        SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT'                TO CPRODT-CTBIL-ELEMT OF TRICB003.
           MOVE GFCTAX-COPER-CTBIL(IND-1)
                                      TO COPER-CTBIL        OF TRICB003.

           EXEC SQL
             SELECT CPRODT_CTBIL_ELEMT,
                    COPER_CTBIL
             INTO  :TRICB003.CPRODT-CTBIL-ELEMT,
                   :TRICB003.COPER-CTBIL
             FROM   DB2PRD.OPERACAO_CTBIL
             WHERE  CPRODT_CTBIL_ELEMT = :TRICB003.CPRODT-CTBIL-ELEMT
               AND  COPER_CTBIL        = :TRICB003.COPER-CTBIL
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL

               MOVE 'GFCT0212'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'OPERACAO_CTBIL'   TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE           TO GFCT0M-SQLCODE
                                          GFCTAY-COD-SQL-ERRO
               MOVE '0040'             TO GFCT0M-LOCAL
               MOVE  SQLCA             TO GFCT0M-SQLCA-AREA
               MOVE  9                 TO GFCTAY-ERRO
               MOVE  12                TO GFCTAY-COD-MSG-ERRO
                                          WRK-5000-COD-MSG

               PERFORM 1110-00-OBTER-DESCRICAO-MSG

               STRING WRK-5000-DESC-MSG
                      ' - TRICB003'
               DELIMITED BY '  '       INTO GFCTAY-DESC-MSG-ERRO

               INITIALIZE GFCTAY-DADOS

               PERFORM 4000-00-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 1                  TO GFCTAY-ERRO
               MOVE ZEROS              TO GFCTAY-COD-SQL-ERRO
               MOVE 117                TO GFCTAY-COD-MSG-ERRO
                                          WRK-5000-COD-MSG

               PERFORM 1110-00-OBTER-DESCRICAO-MSG

               MOVE WRK-5000-DESC-MSG  TO GFCTAY-DESC-MSG-ERRO

               INITIALIZE GFCTAY-DADOS

               PERFORM 4000-00-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2122-00-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2123-00-INSERIR-GFCTB085         SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             INSERT INTO DB2PRD.HIST_EVNTO_CTBIL
                  (CSERVC_TARIF,
                   HIDTFD_MANUT_SERVC,
                   CSEQ_MANUT_SERVC,
                   CTPO_LCTO_CTBIL,
                   CSEQ_EVNTO_CTBIL,
                   CINDCD_DEPDC_CTBIL,
                   PATRIC_CTBIL,
                   CDEPDC_CTLZO,
                   CPRODT_CTBIL,
                   COPER_CTBIL,
                   CEMPR,
                   CGRP_RZ_CTBIL,
                   CSGRP_RZ_CTBIL ,
                   CCTA_EVNTO_CTBIL,
                   CDOCTO_CTBIL)
               VALUES
                  (:GFCTB085.CSERVC-TARIF,
                   :GFCTB085.HIDTFD-MANUT-SERVC,
                   :GFCTB085.CSEQ-MANUT-SERVC,
                   :GFCTB085.CTPO-LCTO-CTBIL,
                   :GFCTB085.CSEQ-EVNTO-CTBIL,
                   :GFCTB085.CINDCD-DEPDC-CTBIL,
                   :GFCTB085.PATRIC-CTBIL,
                   :GFCTB085.CDEPDC-CTLZO,
                   :GFCTB085.CPRODT-CTBIL,
                   :GFCTB085.COPER-CTBIL,
                   :GFCTB085.CEMPR,
                   :GFCTB085.CGRP-RZ-CTBIL,
                   :GFCTB085.CSGRP-RZ-CTBIL,
                   :GFCTB085.CCTA-EVNTO-CTBIL,
                   :GFCTB085.CDOCTO-CTBIL)
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL

               MOVE 'GFCT0212'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'HIST_EVNTO_CTBIL' TO GFCT0M-NOME-TAB
               MOVE 'INSERT'           TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE           TO GFCT0M-SQLCODE
                                          GFCTAY-COD-SQL-ERRO
               MOVE '0050'             TO GFCT0M-LOCAL
               MOVE  SQLCA             TO GFCT0M-SQLCA-AREA
               MOVE  9                 TO GFCTAY-ERRO
               MOVE  18                TO GFCTAY-COD-MSG-ERRO
                                          WRK-5000-COD-MSG

               PERFORM 1110-00-OBTER-DESCRICAO-MSG

               STRING WRK-5000-DESC-MSG
                      ' - GFCTB085'
               DELIMITED BY '  '       INTO GFCTAY-DESC-MSG-ERRO

               INITIALIZE GFCTAY-DADOS

               PERFORM 4000-00-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2123-00-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
CREMM *----------------------------------------------------------------*
CREMM  2124-00-INSERIR-GFCTB047         SECTION.
CREMM *----------------------------------------------------------------*
CREMM
CREMM      MOVE GFCTAX-CSERVC-TARIF       TO CSERVC-TARIF OF GFCTB047.
CREMM      MOVE GFCTAX-HIDTFD-MANUT-SERVC TO HIDTFD-MANUT-SERVC
CREMM                                                     OF GFCTB047.
CREMM      MOVE GFCTAX-CSEQ-MANUT-SERVC   TO CSEQ-MANUT-SERVC
CREMM                                                     OF GFCTB047.
CREMM      MOVE ZEROS                     TO CPRODT-CTBIL OF GFCTB047.
CREMM      MOVE GFCTAX-COPER-CTBIL(IND-1) TO COPER-CTBIL  OF GFCTB047.
CREMM      MOVE GFCTAX-CTPO-LCTO-CTBIL(IND-1)
CREMM                                     TO CTPO-LCTO-CTBIL
CREMM                                                     OF GFCTB047.
CREMM      MOVE GFCTAX-CSEQ-EVNTO-CTBIL(IND-1)
CREMM                                     TO CSEQ-EVNTO-CTBIL
CREMM                                                     OF GFCTB047.
CREMM      MOVE GFCTAX-CINDCD-DEPDC-CTBIL(IND-1)
CREMM                                     TO CINDCD-DEPDC-CTBIL
CREMM                                                     OF GFCTB047.
CREMM      MOVE GFCTAX-PATRIC-CTBIL(IND-1)
CREMM                                     TO PATRIC-CTBIL OF GFCTB047.
CREMM      MOVE GFCTAX-CJUNC-DEPDC-CTRLZ(IND-1)
CREMM                                     TO CDEPDC-CTLZO OF GFCTB047.
CREMM
CREMM      MOVE ZEROS                     TO CEMPR        OF GFCTB047
CREMM                                        CGRP-RZ-CTBIL
CREMM                                                     OF GFCTB047
CREMM                                        CSGRP-RZ-CTBIL
CREMM                                                     OF GFCTB047
CREMM                                        CCTA-EVNTO-CTBIL
CREMM                                                     OF GFCTB047
CREMM                                        CDOCTO-CTBIL OF GFCTB047
CREMM
CREMM      EXEC SQL
CREMM        INSERT INTO DB2PRD.TARIF_EVNTO_CTBIL
CREMM             (CSERVC_TARIF,
CREMM              HIDTFD_MANUT_SERVC,
CREMM              CSEQ_MANUT_SERVC,
CREMM              CTPO_LCTO_CTBIL,
CREMM              CSEQ_EVNTO_CTBIL,
CREMM              CINDCD_DEPDC_CTBIL,
CREMM              PATRIC_CTBIL,
CREMM              CDEPDC_CTLZO,
CREMM              CPRODT_CTBIL,
CREMM              COPER_CTBIL,
CREMM              CEMPR,
CREMM              CGRP_RZ_CTBIL,
CREMM              CSGRP_RZ_CTBIL ,
CREMM              CCTA_EVNTO_CTBIL,
CREMM              CDOCTO_CTBIL)
CREMM          VALUES
CREMM             (:GFCTB047.CSERVC-TARIF,
CREMM              :GFCTB047.HIDTFD-MANUT-SERVC,
CREMM              :GFCTB047.CSEQ-MANUT-SERVC,
CREMM              :GFCTB047.CTPO-LCTO-CTBIL,
CREMM              :GFCTB047.CSEQ-EVNTO-CTBIL,
CREMM              :GFCTB047.CINDCD-DEPDC-CTBIL,
CREMM              :GFCTB047.PATRIC-CTBIL,
CREMM              :GFCTB047.CDEPDC-CTLZO,
CREMM              :GFCTB047.CPRODT-CTBIL,
CREMM              :GFCTB047.COPER-CTBIL,
CREMM              :GFCTB047.CEMPR,
CREMM              :GFCTB047.CGRP-RZ-CTBIL,
CREMM              :GFCTB047.CSGRP-RZ-CTBIL,
CREMM              :GFCTB047.CCTA-EVNTO-CTBIL,
CREMM              :GFCTB047.CDOCTO-CTBIL)
CREMM      END-EXEC.
CREMM
CREMM      IF (SQLCODE                 NOT EQUAL ZEROS) OR
CREMM         (SQLWARN0                EQUAL 'W')
CREMM          INITIALIZE              GFCT0M-ERRO-SQL
CREMM
CREMM          MOVE 'GFCT0212'         TO GFCT0M-PROGRAMA
CREMM          MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
CREMM          MOVE 'TARIF_EVNTO_CTBIL' TO GFCT0M-NOME-TAB
CREMM          MOVE 'INSERT'           TO GFCT0M-COMANDO-SQL
CREMM          MOVE  SQLCODE           TO GFCT0M-SQLCODE
CREMM                                     GFCTAY-COD-SQL-ERRO
CREMM          MOVE '0060'             TO GFCT0M-LOCAL
CREMM          MOVE  SQLCA             TO GFCT0M-SQLCA-AREA
CREMM          MOVE  9                 TO GFCTAY-ERRO
CREMM          MOVE  18                TO GFCTAY-COD-MSG-ERRO
CREMM                                     WRK-5000-COD-MSG
CREMM
CREMM          PERFORM 1110-00-OBTER-DESCRICAO-MSG
CREMM
CREMM          STRING WRK-5000-DESC-MSG
CREMM                 ' - GFCTB047'
CREMM          DELIMITED BY '  '       INTO GFCTAY-DESC-MSG-ERRO
CREMM
CREMM          INITIALIZE GFCTAY-DADOS
CREMM
CREMM          PERFORM 4000-00-FINALIZAR
CREMM      END-IF.
CREMM
CREMM *----------------------------------------------------------------*
CREMM  2124-00-99-FIM.                  EXIT.
CREMM *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       2200-00-ABRIR-CURSOR             SECTION.
      *----------------------------------------------------------------*

           MOVE 'N'                    TO WRK-FIM-CSR.
           MOVE GFCTAX-CSERVC-TARIF    TO CSERVC-TARIF OF GFCTB085.

           COMPUTE CSEQ-MANUT-SERVC    OF GFCTB085
                                       = GFCTAX-CSEQ-MANUT-SERVC - 1.

           EXEC SQL
             OPEN CSR-GFCTB085
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL

               MOVE 'GFCT0212'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'HIST_EVNTO_CTBIL' TO GFCT0M-NOME-TAB
               MOVE 'OPEN'             TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE           TO GFCT0M-SQLCODE
                                          GFCTAY-COD-SQL-ERRO
               MOVE '0070'             TO GFCT0M-LOCAL
               MOVE  SQLCA             TO GFCT0M-SQLCA-AREA
               MOVE  9                 TO GFCTAY-ERRO
               MOVE  5                 TO GFCTAY-COD-MSG-ERRO
                                          WRK-5000-COD-MSG

               PERFORM 1110-00-OBTER-DESCRICAO-MSG

               STRING WRK-5000-DESC-MSG
                      ' - GFCTB085'
               DELIMITED BY '  '       INTO GFCTAY-DESC-MSG-ERRO

               INITIALIZE GFCTAY-DADOS

               PERFORM 4000-00-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2200-00-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2300-00-LER-CURSOR               SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             FETCH  CSR-GFCTB085
             INTO  :GFCTB085.CSERVC-TARIF,
                   :GFCTB085.HIDTFD-MANUT-SERVC,
                   :GFCTB085.CSEQ-MANUT-SERVC,
                   :GFCTB085.CTPO-LCTO-CTBIL,
                   :GFCTB085.CSEQ-EVNTO-CTBIL,
                   :GFCTB085.CINDCD-DEPDC-CTBIL,
                   :GFCTB085.PATRIC-CTBIL,
                   :GFCTB085.CDEPDC-CTLZO,
                   :GFCTB085.CPRODT-CTBIL,
                   :GFCTB085.COPER-CTBIL
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL

               MOVE 'GFCT0212'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'HIST_EVNTO_CTBIL' TO GFCT0M-NOME-TAB
               MOVE 'FETCH'            TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE           TO GFCT0M-SQLCODE
                                          GFCTAY-COD-SQL-ERRO
               MOVE '0080'             TO GFCT0M-LOCAL
               MOVE  SQLCA             TO GFCT0M-SQLCA-AREA
               MOVE  9                 TO GFCTAY-ERRO
               MOVE  6                 TO GFCTAY-COD-MSG-ERRO
                                          WRK-5000-COD-MSG

               PERFORM 1110-00-OBTER-DESCRICAO-MSG

               STRING WRK-5000-DESC-MSG
                      ' - GFCTB085'
               DELIMITED BY '  '       INTO GFCTAY-DESC-MSG-ERRO

               INITIALIZE GFCTAY-DADOS

               PERFORM 4000-00-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FIM-CSR
           ELSE
               MOVE CTPO-LCTO-CTBIL    OF GFCTB085
                                       TO WRK-LANCAMENTO

SP1504         IF  GFCTAX-CPSSOA-SERVC-TARIF
SP1504                                 EQUAL 'A' OR 'F'
                   PERFORM 2410-00-VERIFICAR-COBRANCA-A

                   PERFORM 2510-00-VERIFICAR-ESTORNO-A
SP1504         END-IF

SP1504         IF  GFCTAX-CPSSOA-SERVC-TARIF
SP1504                                 EQUAL 'A' OR 'J'
SP1504             PERFORM 2430-00-VERIFICA-COBRANCA-PJ-A
SP1504
SP1504             PERFORM 2530-00-VERIFICAR-ESTORNO-PJ-A
SP1504         END-IF
           END-IF.

      *----------------------------------------------------------------*
       2300-00-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       2400-00-VERIFICAR-COBRANCA       SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTAX-CSERVC-TARIF   TO CSERVC-TARIF       OF GFCTB085.
           MOVE GFCTAX-HIDTFD-MANUT-SERVC
                                      TO HIDTFD-MANUT-SERVC OF GFCTB085.
           MOVE GFCTAX-CSEQ-MANUT-SERVC
                                      TO CSEQ-MANUT-SERVC   OF GFCTB085.
           MOVE GFCTAX-CTPO-LCTO-COBR TO CTPO-LCTO-CTBIL    OF GFCTB085.

           EXEC SQL
             SELECT CSERVC_TARIF
             INTO  :GFCTB085.CSERVC-TARIF
             FROM   DB2PRD.HIST_EVNTO_CTBIL
             WHERE  CSERVC_TARIF       = :GFCTB085.CSERVC-TARIF
               AND  HIDTFD_MANUT_SERVC = :GFCTB085.HIDTFD-MANUT-SERVC
               AND  CSEQ_MANUT_SERVC   = :GFCTB085.CSEQ-MANUT-SERVC
               AND  CTPO_LCTO_CTBIL    = :GFCTB085.CTPO-LCTO-CTBIL
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100
                                                       AND -811) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL

               MOVE 'GFCT0212'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'HIST_EVNTO_CTBIL' TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE           TO GFCT0M-SQLCODE
                                          GFCTAY-COD-SQL-ERRO
               MOVE '0090'             TO GFCT0M-LOCAL
               MOVE  SQLCA             TO GFCT0M-SQLCA-AREA
               MOVE  9                 TO GFCTAY-ERRO
               MOVE  12                TO GFCTAY-COD-MSG-ERRO
                                          WRK-5000-COD-MSG

               PERFORM 1110-00-OBTER-DESCRICAO-MSG

               STRING WRK-5000-DESC-MSG
                      ' - GFCTB085'
               DELIMITED BY '  '       INTO GFCTAY-DESC-MSG-ERRO

               INITIALIZE GFCTAY-DADOS

               PERFORM 4000-00-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 1                  TO GFCTAY-ERRO
               MOVE SQLCODE            TO GFCTAY-COD-SQL-ERRO
               MOVE 211                TO GFCTAY-COD-MSG-ERRO
                                          WRK-5000-COD-MSG

               PERFORM 1110-00-OBTER-DESCRICAO-MSG

               MOVE WRK-5000-DESC-MSG  TO GFCTAY-DESC-MSG-ERRO

               INITIALIZE GFCTAY-DADOS

               PERFORM 4000-00-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2400-00-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2410-00-VERIFICAR-COBRANCA-A     SECTION.
      *----------------------------------------------------------------*

           COMPUTE WRK-SEQANT = GFCTAX-CSEQ-MANUT-SERVC - 1
           MOVE WRK-SEQANT            TO CSEQ-MANUT-SERVC   OF GFCTB085.

           MOVE GFCTAX-CSERVC-TARIF   TO CSERVC-TARIF       OF GFCTB085.
           MOVE GFCTAX-CTPO-LCTO-COBR TO CTPO-LCTO-CTBIL    OF GFCTB085.

           EXEC SQL
             SELECT CSERVC_TARIF
             INTO  :GFCTB085.CSERVC-TARIF
             FROM   DB2PRD.HIST_EVNTO_CTBIL
             WHERE  CSERVC_TARIF       = :GFCTB085.CSERVC-TARIF
               AND  CSEQ_MANUT_SERVC   = :GFCTB085.CSEQ-MANUT-SERVC
               AND  CTPO_LCTO_CTBIL    = :GFCTB085.CTPO-LCTO-CTBIL
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100
                                                       AND -811) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL

               MOVE 'GFCT0212'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'HIST_EVNTO_CTBIL' TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE           TO GFCT0M-SQLCODE
                                          GFCTAY-COD-SQL-ERRO
               MOVE '0100'             TO GFCT0M-LOCAL
               MOVE  SQLCA             TO GFCT0M-SQLCA-AREA
               MOVE  9                 TO GFCTAY-ERRO
               MOVE  12                TO GFCTAY-COD-MSG-ERRO
                                          WRK-5000-COD-MSG

               PERFORM 1110-00-OBTER-DESCRICAO-MSG

               STRING WRK-5000-DESC-MSG
                      ' - GFCTB085'
               DELIMITED BY '  '       INTO GFCTAY-DESC-MSG-ERRO

               INITIALIZE GFCTAY-DADOS

               PERFORM 4000-00-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 1                  TO GFCTAY-ERRO
               MOVE SQLCODE            TO GFCTAY-COD-SQL-ERRO
               MOVE 211                TO GFCTAY-COD-MSG-ERRO
                                          WRK-5000-COD-MSG

               PERFORM 1110-00-OBTER-DESCRICAO-MSG

               MOVE WRK-5000-DESC-MSG  TO GFCTAY-DESC-MSG-ERRO

               INITIALIZE GFCTAY-DADOS

               PERFORM 4000-00-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2410-00-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

SP1504*----------------------------------------------------------------*
SP1504 2420-00-VERIFICAR-COBRANCA-PJ    SECTION.
SP1504*----------------------------------------------------------------*
SP1504
SP1504     MOVE GFCTAX-CSERVC-TARIF   TO CSERVC-TARIF       OF GFCTB085.
SP1504     MOVE GFCTAX-HIDTFD-MANUT-SERVC
SP1504                                TO HIDTFD-MANUT-SERVC OF GFCTB085.
SP1504     MOVE GFCTAX-CSEQ-MANUT-SERVC
SP1504                                TO CSEQ-MANUT-SERVC   OF GFCTB085.
SP1504     MOVE GFCTAX-CTPO-LCTO-COBR-PJ
SP1504                                TO CTPO-LCTO-CTBIL    OF GFCTB085.
SP1504
SP1504     EXEC SQL
SP1504       SELECT CSERVC_TARIF
SP1504       INTO  :GFCTB085.CSERVC-TARIF
SP1504       FROM   DB2PRD.HIST_EVNTO_CTBIL
SP1504       WHERE  CSERVC_TARIF       = :GFCTB085.CSERVC-TARIF
SP1504         AND  HIDTFD_MANUT_SERVC = :GFCTB085.HIDTFD-MANUT-SERVC
SP1504         AND  CSEQ_MANUT_SERVC   = :GFCTB085.CSEQ-MANUT-SERVC
SP1504         AND  CTPO_LCTO_CTBIL    = :GFCTB085.CTPO-LCTO-CTBIL
SP1504     END-EXEC.
SP1504
SP1504     IF (SQLCODE                 NOT EQUAL ZEROS AND +100
SP1504                                                 AND -811) OR
SP1504        (SQLWARN0                EQUAL 'W')
SP1504         INITIALIZE              GFCT0M-ERRO-SQL
SP1504
SP1504         MOVE 'GFCT0212'         TO GFCT0M-PROGRAMA
SP1504         MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
SP1504         MOVE 'HIST_EVNTO_CTBIL' TO GFCT0M-NOME-TAB
SP1504         MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
SP1504         MOVE  SQLCODE           TO GFCT0M-SQLCODE
SP1504                                    GFCTAY-COD-SQL-ERRO
SP1504         MOVE '0110'             TO GFCT0M-LOCAL
SP1504         MOVE  SQLCA             TO GFCT0M-SQLCA-AREA
SP1504         MOVE  9                 TO GFCTAY-ERRO
SP1504         MOVE  12                TO GFCTAY-COD-MSG-ERRO
SP1504                                    WRK-5000-COD-MSG
SP1504
SP1504         PERFORM 1110-00-OBTER-DESCRICAO-MSG
SP1504
SP1504         STRING WRK-5000-DESC-MSG
SP1504                ' - GFCTB085'
SP1504         DELIMITED BY '  '       INTO GFCTAY-DESC-MSG-ERRO
SP1504
SP1504         INITIALIZE GFCTAY-DADOS
SP1504
SP1504         PERFORM 4000-00-FINALIZAR
SP1504     END-IF.
SP1504
SP1504     IF  SQLCODE                 EQUAL +100
SP1504         MOVE 1                  TO GFCTAY-ERRO
SP1504         MOVE SQLCODE            TO GFCTAY-COD-SQL-ERRO
SP1504         MOVE 1419               TO GFCTAY-COD-MSG-ERRO
SP1504                                    WRK-5000-COD-MSG
SP1504
SP1504         PERFORM 1110-00-OBTER-DESCRICAO-MSG
SP1504
SP1504         MOVE WRK-5000-DESC-MSG  TO GFCTAY-DESC-MSG-ERRO
SP1504
SP1504         INITIALIZE GFCTAY-DADOS
SP1504
SP1504         PERFORM 4000-00-FINALIZAR
SP1504     END-IF.
SP1504
SP1504*----------------------------------------------------------------*
SP1504 2420-00-99-FIM.                  EXIT.
SP1504*----------------------------------------------------------------*
SP1504
SP1504*----------------------------------------------------------------*
SP1504 2430-00-VERIFICA-COBRANCA-PJ-A  SECTION.
SP1504*----------------------------------------------------------------*
SP1504
SP1504     COMPUTE WRK-SEQANT = GFCTAX-CSEQ-MANUT-SERVC - 1
SP1504     MOVE WRK-SEQANT            TO CSEQ-MANUT-SERVC   OF GFCTB085.
SP1504
SP1504     MOVE GFCTAX-CSERVC-TARIF   TO CSERVC-TARIF       OF GFCTB085.
SP1504     MOVE GFCTAX-CTPO-LCTO-COBR-PJ
SP1504                                TO CTPO-LCTO-CTBIL    OF GFCTB085.
SP1504
SP1504     EXEC SQL
SP1504       SELECT CSERVC_TARIF
SP1504       INTO  :GFCTB085.CSERVC-TARIF
SP1504       FROM   DB2PRD.HIST_EVNTO_CTBIL
SP1504       WHERE  CSERVC_TARIF       = :GFCTB085.CSERVC-TARIF
SP1504         AND  CSEQ_MANUT_SERVC   = :GFCTB085.CSEQ-MANUT-SERVC
SP1504         AND  CTPO_LCTO_CTBIL    = :GFCTB085.CTPO-LCTO-CTBIL
SP1504     END-EXEC.
SP1504
SP1504     IF (SQLCODE                 NOT EQUAL ZEROS AND +100
SP1504                                                 AND -811) OR
SP1504        (SQLWARN0                EQUAL 'W')
SP1504         INITIALIZE              GFCT0M-ERRO-SQL
SP1504
SP1504         MOVE 'GFCT0212'         TO GFCT0M-PROGRAMA
SP1504         MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
SP1504         MOVE 'HIST_EVNTO_CTBIL' TO GFCT0M-NOME-TAB
SP1504         MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
SP1504         MOVE  SQLCODE           TO GFCT0M-SQLCODE
SP1504                                    GFCTAY-COD-SQL-ERRO
SP1504         MOVE '0120'             TO GFCT0M-LOCAL
SP1504         MOVE  SQLCA             TO GFCT0M-SQLCA-AREA
SP1504         MOVE  9                 TO GFCTAY-ERRO
SP1504         MOVE  12                TO GFCTAY-COD-MSG-ERRO
SP1504                                    WRK-5000-COD-MSG
SP1504
SP1504         PERFORM 1110-00-OBTER-DESCRICAO-MSG
SP1504
SP1504         STRING WRK-5000-DESC-MSG
SP1504                ' - GFCTB085'
SP1504         DELIMITED BY '  '       INTO GFCTAY-DESC-MSG-ERRO
SP1504
SP1504         INITIALIZE GFCTAY-DADOS
SP1504
SP1504         PERFORM 4000-00-FINALIZAR
SP1504     END-IF.
SP1504
SP1504     IF  SQLCODE                 EQUAL +100
SP1504         MOVE 1                  TO GFCTAY-ERRO
SP1504         MOVE SQLCODE            TO GFCTAY-COD-SQL-ERRO
SP1504         MOVE 1419               TO GFCTAY-COD-MSG-ERRO
SP1504                                    WRK-5000-COD-MSG
SP1504
SP1504         PERFORM 1110-00-OBTER-DESCRICAO-MSG
SP1504
SP1504         MOVE WRK-5000-DESC-MSG  TO GFCTAY-DESC-MSG-ERRO
SP1504
SP1504         INITIALIZE GFCTAY-DADOS
SP1504
SP1504         PERFORM 4000-00-FINALIZAR
SP1504     END-IF.
SP1504
SP1504*----------------------------------------------------------------*
SP1504 2430-00-99-FIM.                  EXIT.
SP1504*----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2500-00-VERIFICAR-ESTORNO        SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTAX-CSERVC-TARIF   TO CSERVC-TARIF       OF GFCTB085.
           MOVE GFCTAX-HIDTFD-MANUT-SERVC
                                      TO HIDTFD-MANUT-SERVC OF GFCTB085.
           MOVE GFCTAX-CSEQ-MANUT-SERVC
                                      TO CSEQ-MANUT-SERVC   OF GFCTB085.
           MOVE GFCTAX-CTPO-LCTO-ESTOR
                                      TO CTPO-LCTO-CTBIL    OF GFCTB085.

           EXEC SQL
             SELECT CSERVC_TARIF
             INTO  :GFCTB085.CSERVC-TARIF
             FROM   DB2PRD.HIST_EVNTO_CTBIL
             WHERE  CSERVC_TARIF       = :GFCTB085.CSERVC-TARIF
               AND  HIDTFD_MANUT_SERVC = :GFCTB085.HIDTFD-MANUT-SERVC
               AND  CSEQ_MANUT_SERVC   = :GFCTB085.CSEQ-MANUT-SERVC
               AND  CTPO_LCTO_CTBIL    = :GFCTB085.CTPO-LCTO-CTBIL
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100
                                                       AND -811) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL

               MOVE 'GFCT0212'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'HIST_EVNTO_CTBIL' TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE           TO GFCT0M-SQLCODE
                                          GFCTAY-COD-SQL-ERRO
               MOVE '0130'             TO GFCT0M-LOCAL
               MOVE  SQLCA             TO GFCT0M-SQLCA-AREA
               MOVE  9                 TO GFCTAY-ERRO
               MOVE  12                TO GFCTAY-COD-MSG-ERRO
                                          WRK-5000-COD-MSG

               PERFORM 1110-00-OBTER-DESCRICAO-MSG

               STRING WRK-5000-DESC-MSG
                      ' - GFCTB085'
               DELIMITED BY '  '       INTO GFCTAY-DESC-MSG-ERRO

               INITIALIZE GFCTAY-DADOS

               PERFORM 4000-00-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 1                  TO GFCTAY-ERRO
               MOVE SQLCODE            TO GFCTAY-COD-SQL-ERRO
               MOVE 212                TO GFCTAY-COD-MSG-ERRO
                                          WRK-5000-COD-MSG

               PERFORM 1110-00-OBTER-DESCRICAO-MSG

               MOVE WRK-5000-DESC-MSG  TO GFCTAY-DESC-MSG-ERRO

               INITIALIZE GFCTAY-DADOS

               PERFORM 4000-00-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2500-00-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2510-00-VERIFICAR-ESTORNO-A      SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTAX-CSERVC-TARIF   TO CSERVC-TARIF       OF GFCTB085.
           MOVE GFCTAX-CTPO-LCTO-ESTOR
                                      TO CTPO-LCTO-CTBIL    OF GFCTB085.

           EXEC SQL
             SELECT CSERVC_TARIF
             INTO  :GFCTB085.CSERVC-TARIF
             FROM   DB2PRD.HIST_EVNTO_CTBIL
             WHERE  CSERVC_TARIF       = :GFCTB085.CSERVC-TARIF
               AND  CSEQ_MANUT_SERVC   = :GFCTB085.CSEQ-MANUT-SERVC
               AND  CTPO_LCTO_CTBIL    = :GFCTB085.CTPO-LCTO-CTBIL
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100
                                                       AND -811) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL

               MOVE 'GFCT0212'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'HIST_EVNTO_CTBIL' TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE           TO GFCT0M-SQLCODE
                                          GFCTAY-COD-SQL-ERRO
               MOVE '0140'             TO GFCT0M-LOCAL
               MOVE  SQLCA             TO GFCT0M-SQLCA-AREA
               MOVE  9                 TO GFCTAY-ERRO
               MOVE  12                TO GFCTAY-COD-MSG-ERRO
                                          WRK-5000-COD-MSG

               PERFORM 1110-00-OBTER-DESCRICAO-MSG

               STRING WRK-5000-DESC-MSG
                      ' - GFCTB085'
               DELIMITED BY '  '       INTO GFCTAY-DESC-MSG-ERRO

               INITIALIZE GFCTAY-DADOS

               PERFORM 4000-00-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 1                  TO GFCTAY-ERRO
               MOVE SQLCODE            TO GFCTAY-COD-SQL-ERRO
               MOVE 212                TO GFCTAY-COD-MSG-ERRO
                                          WRK-5000-COD-MSG

               PERFORM 1110-00-OBTER-DESCRICAO-MSG

               MOVE WRK-5000-DESC-MSG  TO GFCTAY-DESC-MSG-ERRO

               INITIALIZE GFCTAY-DADOS

               PERFORM 4000-00-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2510-00-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

SP1504*----------------------------------------------------------------*
SP1504 2520-00-VERIFICAR-ESTORNO-PJ     SECTION.
SP1504*----------------------------------------------------------------*
SP1504
SP1504     MOVE GFCTAX-CSERVC-TARIF   TO CSERVC-TARIF       OF GFCTB085.
SP1504     MOVE GFCTAX-HIDTFD-MANUT-SERVC
SP1504                                TO HIDTFD-MANUT-SERVC OF GFCTB085.
SP1504     MOVE GFCTAX-CSEQ-MANUT-SERVC
SP1504                                TO CSEQ-MANUT-SERVC   OF GFCTB085.
SP1504     MOVE GFCTAX-CTPO-LCTO-ESTOR-PJ
SP1504                                TO CTPO-LCTO-CTBIL    OF GFCTB085.
SP1504
SP1504     EXEC SQL
SP1504       SELECT CSERVC_TARIF
SP1504       INTO  :GFCTB085.CSERVC-TARIF
SP1504       FROM   DB2PRD.HIST_EVNTO_CTBIL
SP1504       WHERE  CSERVC_TARIF       = :GFCTB085.CSERVC-TARIF
SP1504         AND  HIDTFD_MANUT_SERVC = :GFCTB085.HIDTFD-MANUT-SERVC
SP1504         AND  CSEQ_MANUT_SERVC   = :GFCTB085.CSEQ-MANUT-SERVC
SP1504         AND  CTPO_LCTO_CTBIL    = :GFCTB085.CTPO-LCTO-CTBIL
SP1504     END-EXEC.
SP1504
SP1504     IF (SQLCODE                 NOT EQUAL ZEROS AND +100
SP1504                                                 AND -811) OR
SP1504        (SQLWARN0                EQUAL 'W')
SP1504         INITIALIZE              GFCT0M-ERRO-SQL
SP1504
SP1504         MOVE 'GFCT0212'         TO GFCT0M-PROGRAMA
SP1504         MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
SP1504         MOVE 'HIST_EVNTO_CTBIL' TO GFCT0M-NOME-TAB
SP1504         MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
SP1504         MOVE  SQLCODE           TO GFCT0M-SQLCODE
SP1504                                    GFCTAY-COD-SQL-ERRO
SP1504         MOVE '0150'             TO GFCT0M-LOCAL
SP1504         MOVE  SQLCA             TO GFCT0M-SQLCA-AREA
SP1504         MOVE  9                 TO GFCTAY-ERRO
SP1504         MOVE  12                TO GFCTAY-COD-MSG-ERRO
SP1504                                    WRK-5000-COD-MSG
SP1504
SP1504         PERFORM 1110-00-OBTER-DESCRICAO-MSG
SP1504
SP1504         STRING WRK-5000-DESC-MSG
SP1504                ' - GFCTB085'
SP1504         DELIMITED BY '  '       INTO GFCTAY-DESC-MSG-ERRO
SP1504
SP1504         INITIALIZE GFCTAY-DADOS
SP1504
SP1504         PERFORM 4000-00-FINALIZAR
SP1504     END-IF.
SP1504
SP1504     IF  SQLCODE                 EQUAL +100
SP1504         MOVE 1                  TO GFCTAY-ERRO
SP1504         MOVE SQLCODE            TO GFCTAY-COD-SQL-ERRO
SP1504         MOVE 1420               TO GFCTAY-COD-MSG-ERRO
SP1504                                    WRK-5000-COD-MSG
SP1504
SP1504         PERFORM 1110-00-OBTER-DESCRICAO-MSG
SP1504
SP1504         MOVE WRK-5000-DESC-MSG  TO GFCTAY-DESC-MSG-ERRO
SP1504
SP1504         INITIALIZE GFCTAY-DADOS
SP1504
SP1504         PERFORM 4000-00-FINALIZAR
SP1504     END-IF.
SP1504
SP1504*----------------------------------------------------------------*
SP1504 2520-00-99-FIM.                  EXIT.
SP1504*----------------------------------------------------------------*
SP1504
SP1504*----------------------------------------------------------------*
SP1504 2530-00-VERIFICAR-ESTORNO-PJ-A   SECTION.
SP1504*----------------------------------------------------------------*
SP1504
SP1504     MOVE GFCTAX-CSERVC-TARIF   TO CSERVC-TARIF       OF GFCTB085.
SP1504     MOVE GFCTAX-CTPO-LCTO-ESTOR-PJ
SP1504                                TO CTPO-LCTO-CTBIL    OF GFCTB085.
SP1504
SP1504     EXEC SQL
SP1504       SELECT CSERVC_TARIF
SP1504       INTO  :GFCTB085.CSERVC-TARIF
SP1504       FROM   DB2PRD.HIST_EVNTO_CTBIL
SP1504       WHERE  CSERVC_TARIF       = :GFCTB085.CSERVC-TARIF
SP1504         AND  CSEQ_MANUT_SERVC   = :GFCTB085.CSEQ-MANUT-SERVC
SP1504         AND  CTPO_LCTO_CTBIL    = :GFCTB085.CTPO-LCTO-CTBIL
SP1504     END-EXEC.
SP1504
SP1504     IF (SQLCODE                 NOT EQUAL ZEROS AND +100
SP1504                                                 AND -811) OR
SP1504        (SQLWARN0                EQUAL 'W')
SP1504         INITIALIZE              GFCT0M-ERRO-SQL
SP1504
SP1504         MOVE 'GFCT0212'         TO GFCT0M-PROGRAMA
SP1504         MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
SP1504         MOVE 'HIST_EVNTO_CTBIL' TO GFCT0M-NOME-TAB
SP1504         MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
SP1504         MOVE  SQLCODE           TO GFCT0M-SQLCODE
SP1504                                    GFCTAY-COD-SQL-ERRO
SP1504         MOVE '0160'             TO GFCT0M-LOCAL
SP1504         MOVE  SQLCA             TO GFCT0M-SQLCA-AREA
SP1504         MOVE  9                 TO GFCTAY-ERRO
SP1504         MOVE  12                TO GFCTAY-COD-MSG-ERRO
SP1504                                    WRK-5000-COD-MSG
SP1504
SP1504         PERFORM 1110-00-OBTER-DESCRICAO-MSG
SP1504
SP1504         STRING WRK-5000-DESC-MSG
SP1504                ' - GFCTB085'
SP1504         DELIMITED BY '  '       INTO GFCTAY-DESC-MSG-ERRO
SP1504
SP1504         INITIALIZE GFCTAY-DADOS
SP1504
SP1504         PERFORM 4000-00-FINALIZAR
SP1504     END-IF.
SP1504
SP1504     IF  SQLCODE                 EQUAL +100
SP1504         MOVE 1                  TO GFCTAY-ERRO
SP1504         MOVE SQLCODE            TO GFCTAY-COD-SQL-ERRO
SP1504         MOVE 1420               TO GFCTAY-COD-MSG-ERRO
SP1504                                    WRK-5000-COD-MSG
SP1504
SP1504         PERFORM 1110-00-OBTER-DESCRICAO-MSG
SP1504
SP1504         MOVE WRK-5000-DESC-MSG  TO GFCTAY-DESC-MSG-ERRO
SP1504
SP1504         INITIALIZE GFCTAY-DADOS
SP1504
SP1504         PERFORM 4000-00-FINALIZAR
SP1504     END-IF.
SP1504
SP1504*----------------------------------------------------------------*
SP1504 2530-00-99-FIM.                  EXIT.
SP1504*----------------------------------------------------------------*
           EJECT
BI0411*----------------------------------------------------------------*
BI0411 2540-00-VERIFICA-ESTORNO-ANTER  SECTION.
BI0411*----------------------------------------------------------------*
BI0411
BI0411     MOVE GFCTAX-CSERVC-TARIF   TO CSERVC-TARIF       OF GFCTB085.
BI0411     MOVE GFCTAX-CLCTO-ESTRN-ANTER
BI0411                                TO CTPO-LCTO-CTBIL    OF GFCTB085.
BI0411
BI0411     EXEC SQL
BI0411       SELECT CSERVC_TARIF
BI0411       INTO  :GFCTB085.CSERVC-TARIF
BI0411       FROM   DB2PRD.HIST_EVNTO_CTBIL
BI0411       WHERE  CSERVC_TARIF       = :GFCTB085.CSERVC-TARIF
BI0411         AND  CSEQ_MANUT_SERVC   = :GFCTB085.CSEQ-MANUT-SERVC
BI0411         AND  CTPO_LCTO_CTBIL    = :GFCTB085.CTPO-LCTO-CTBIL
BI0411     END-EXEC.
BI0411
BI0411     IF (SQLCODE                 NOT EQUAL ZEROS AND +100
BI0411                                                 AND -811) OR
BI0411        (SQLWARN0                EQUAL 'W')
BI0411         INITIALIZE              GFCT0M-ERRO-SQL
BI0411
BI0411         MOVE 'GFCT0212'         TO GFCT0M-PROGRAMA
BI0411         MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
BI0411         MOVE 'HIST_EVNTO_CTBIL' TO GFCT0M-NOME-TAB
BI0411         MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
BI0411         MOVE SQLCODE            TO GFCT0M-SQLCODE
BI0411                                    WRK-S9-3
BI0411         MOVE WRK-3-9            TO GFCTAY-COD-SQL-ERRO
BI0411         MOVE '0170'             TO GFCT0M-LOCAL
BI0411         MOVE  SQLCA             TO GFCT0M-SQLCA-AREA
BI0411         MOVE  9                 TO GFCTAY-ERRO
BI0411         MOVE  12                TO GFCTAY-COD-MSG-ERRO
BI0411                                    WRK-5000-COD-MSG
BI0411
BI0411         PERFORM 1110-00-OBTER-DESCRICAO-MSG
BI0411
BI0411         STRING WRK-5000-DESC-MSG
BI0411                ' - GFCTB085'
BI0411         DELIMITED BY '  '       INTO GFCTAY-DESC-MSG-ERRO
BI0411
BI0411         INITIALIZE GFCTAY-DADOS
BI0411
BI0411         PERFORM 4000-00-FINALIZAR
BI0411     END-IF.
BI0411
BI0411     IF  SQLCODE                 EQUAL +100
BI0411         MOVE 1                  TO GFCTAY-ERRO
BI0411         MOVE SQLCODE            TO GFCT0M-SQLCODE
BI0411                                    WRK-S9-3
BI0411         MOVE WRK-3-9            TO GFCTAY-COD-SQL-ERRO
BI0411         MOVE 1837               TO GFCTAY-COD-MSG-ERRO
BI0411                                    WRK-5000-COD-MSG
BI0411
BI0411         PERFORM 1110-00-OBTER-DESCRICAO-MSG
BI0411
BI0411         MOVE WRK-5000-DESC-MSG  TO GFCTAY-DESC-MSG-ERRO
BI0411
BI0411         INITIALIZE GFCTAY-DADOS
BI0411
BI0411         PERFORM 4000-00-FINALIZAR
BI0411     END-IF.
BI0411
BI0411*----------------------------------------------------------------*
BI0411 2540-00-99-FIM.                  EXIT.
BI0411*----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2600-00-ALTERAR-HIST-PRINC       SECTION.
      *----------------------------------------------------------------*

           PERFORM 2610-00-OBTER-SITUACAO.

           IF  GFCTAX-MASTER           EQUAL 'S'
               IF CSIT-LIBRC-TARIF     OF GFCTB087
                                       GREATER  15
                  MOVE 27              TO CSIT-LIBRC-TARIF OF GFCTB087
               ELSE
                  MOVE  7              TO CSIT-LIBRC-TARIF OF GFCTB087
               END-IF
           ELSE
               MOVE  5                 TO CSIT-LIBRC-TARIF OF GFCTB087
           END-IF.

           MOVE GFCTAX-CTPO-LCTO-COBR  TO CTPO-LCTO-COBR   OF GFCTB087.
           MOVE GFCTAX-CTPO-LCTO-ESTOR TO CTPO-LCTO-ESTRN  OF GFCTB087.
SP1504     MOVE GFCTAX-CTPO-LCTO-COBR-PJ
SP1504                                 TO CLCTO-COBR-PJ    OF GFCTB087.
SP1504     MOVE GFCTAX-CTPO-LCTO-ESTOR-PJ
SP1504                                 TO CLCTO-ESTRN-PJ   OF GFCTB087.
BI0411     MOVE GFCTAX-CLCTO-ESTRN-ANTER
BI0411                              TO CLCTO-ESTRN-ANTER   OF GFCTB087.
           MOVE GFCTAX-CSERVC-TARIF    TO CSERVC-TARIF     OF GFCTB087.
           MOVE GFCTAX-HIDTFD-MANUT-SERVC
                                       TO HIDTFD-MANUT-SERVC
                                                           OF GFCTB087.
           MOVE GFCTAX-CSEQ-MANUT-SERVC
                                       TO CSEQ-MANUT-SERVC OF GFCTB087.
           MOVE 01                     TO CTPO-MANUT-SERVC OF GFCTB087.

           EXEC SQL
             UPDATE DB2PRD.HIST_TARIF_PRINC
                SET CSIT_LIBRC_TARIF   = :GFCTB087.CSIT-LIBRC-TARIF,
                    CTPO_LCTO_COBR     = :GFCTB087.CTPO-LCTO-COBR,
                    CTPO_LCTO_ESTRN    = :GFCTB087.CTPO-LCTO-ESTRN,
SP1204              CLCTO_COBR_PJ      = :GFCTB087.CLCTO-COBR-PJ,
SP1204              CLCTO_ESTRN_PJ     = :GFCTB087.CLCTO-ESTRN-PJ,
BI0411              CLCTO_ESTRN_ANTER  = :GFCTB087.CLCTO-ESTRN-ANTER,
                    CTPO_MANUT_SERVC   = :GFCTB087.CTPO-MANUT-SERVC
             WHERE  CSERVC_TARIF       = :GFCTB087.CSERVC-TARIF
               AND  HIDTFD_MANUT_SERVC = :GFCTB087.HIDTFD-MANUT-SERVC
               AND  CSEQ_MANUT_SERVC   = :GFCTB087.CSEQ-MANUT-SERVC
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL

               MOVE 'GFCT0212'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'HIST_TARIF_PRINC' TO GFCT0M-NOME-TAB
               MOVE 'UPDATE'           TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE           TO GFCT0M-SQLCODE
                                          GFCTAY-COD-SQL-ERRO
               MOVE '0180'             TO GFCT0M-LOCAL
               MOVE  SQLCA             TO GFCT0M-SQLCA-AREA
               MOVE  9                 TO GFCTAY-ERRO
               MOVE  19                TO GFCTAY-COD-MSG-ERRO
                                          WRK-5000-COD-MSG

               PERFORM 1110-00-OBTER-DESCRICAO-MSG

               STRING WRK-5000-DESC-MSG
                      ' - GFCTB087'
               DELIMITED BY '  '       INTO GFCTAY-DESC-MSG-ERRO

               INITIALIZE GFCTAY-DADOS

               PERFORM 4000-00-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 1                  TO GFCTAY-ERRO
               MOVE SQLCODE            TO GFCTAY-COD-SQL-ERRO
               MOVE 10                 TO GFCTAY-COD-MSG-ERRO
                                          WRK-5000-COD-MSG

               PERFORM 1110-00-OBTER-DESCRICAO-MSG

               MOVE WRK-5000-DESC-MSG  TO GFCTAY-DESC-MSG-ERRO

               INITIALIZE GFCTAY-DADOS

               PERFORM 4000-00-FINALIZAR
           END-IF.

SP0704*
SP0704*--- CANCELAR AS VERSOES ANTERIORES DE HISTORICOS DA TARIFA,
SP0704*--- QUE ESTEJAM COM CSIT-LIBRC-TARIF IGUAL A 04 OU 05 OU 06
SP0704*
SP0704     PERFORM 2630-00-CANCELAR-VERSOES-ANTER.

CREMM      IF  GFCTAX-MASTER   EQUAL 'S' AND
CREMM          W-TEM-FUTURA    EQUAL 'S'
CREMM          PERFORM 2620-00-UPDATE-GFCTB0D8
CREMM      END-IF.

      *----------------------------------------------------------------*
       2600-00-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2610-00-OBTER-SITUACAO           SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTAX-CSERVC-TARIF   TO CSERVC-TARIF       OF GFCTB087.
           MOVE GFCTAX-HIDTFD-MANUT-SERVC
                                      TO HIDTFD-MANUT-SERVC OF GFCTB087.
           MOVE GFCTAX-CSEQ-MANUT-SERVC
                                      TO CSEQ-MANUT-SERVC   OF GFCTB087.

           EXEC SQL
             SELECT CSIT_LIBRC_TARIF
             INTO  :GFCTB087.CSIT-LIBRC-TARIF
             FROM   DB2PRD.HIST_TARIF_PRINC
             WHERE  CSERVC_TARIF       = :GFCTB087.CSERVC-TARIF
               AND  HIDTFD_MANUT_SERVC = :GFCTB087.HIDTFD-MANUT-SERVC
               AND  CSEQ_MANUT_SERVC   = :GFCTB087.CSEQ-MANUT-SERVC
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL

               MOVE 'GFCT0212'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'HIST_TARIF_PRINC' TO GFCT0M-NOME-TAB
               MOVE 'SELECT'           TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE           TO GFCT0M-SQLCODE
                                          GFCTAY-COD-SQL-ERRO
               MOVE '0190'             TO GFCT0M-LOCAL
               MOVE  SQLCA             TO GFCT0M-SQLCA-AREA
               MOVE  9                 TO GFCTAY-ERRO
               MOVE  12                TO GFCTAY-COD-MSG-ERRO
                                          WRK-5000-COD-MSG

               PERFORM 1110-00-OBTER-DESCRICAO-MSG

               STRING WRK-5000-DESC-MSG
                      ' - GFCTB087'
               DELIMITED BY '  '       INTO GFCTAY-DESC-MSG-ERRO

               INITIALIZE GFCTAY-DADOS

               PERFORM 4000-00-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 1                  TO GFCTAY-ERRO
               MOVE SQLCODE            TO GFCTAY-COD-SQL-ERRO
               MOVE 54                 TO GFCTAY-COD-MSG-ERRO
                                          WRK-5000-COD-MSG

               PERFORM 1110-00-OBTER-DESCRICAO-MSG

               MOVE WRK-5000-DESC-MSG  TO GFCTAY-DESC-MSG-ERRO

               INITIALIZE GFCTAY-DADOS

               PERFORM 4000-00-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2610-00-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

CREMM *----------------------------------------------------------------*
CREMM  2620-00-UPDATE-GFCTB0D8          SECTION.
CREMM *----------------------------------------------------------------*
CREMM
CREMM      MOVE GFCTAX-CTPO-LCTO-COBR  TO CTPO-LCTO-COBR   OF GFCTB0D8.
CREMM      MOVE GFCTAX-CTPO-LCTO-ESTOR TO CTPO-LCTO-ESTRN  OF GFCTB0D8.
SP1204     MOVE GFCTAX-CTPO-LCTO-COBR-PJ
SP1204                                 TO CLCTO-COBR-PJ    OF GFCTB0D8.
SP1204     MOVE GFCTAX-CTPO-LCTO-ESTOR-PJ
SP1204                                 TO CLCTO-ESTRN-PJ   OF GFCTB0D8.
BI0411     MOVE GFCTAX-CLCTO-ESTRN-ANTER
BI0411                              TO CLCTO-ESTRN-ANTER   OF GFCTB0D8.
CREMM      MOVE GFCTAX-CSERVC-TARIF    TO CSERVC-TARIF     OF GFCTB0D8.
CREMM
CREMM      EXEC SQL
CREMM        UPDATE DB2PRD.SERVC_TARIF_PRINC
CREMM           SET CTPO_LCTO_COBR     = :GFCTB0D8.CTPO-LCTO-COBR,
CREMM               CTPO_LCTO_ESTRN    = :GFCTB0D8.CTPO-LCTO-ESTRN,
SP1204              CLCTO_COBR_PJ      = :GFCTB0D8.CLCTO-COBR-PJ,
SP1204              CLCTO_ESTRN_PJ     = :GFCTB0D8.CLCTO-ESTRN-PJ,
BI0411              CLCTO_ESTRN_ANTER  = :GFCTB0D8.CLCTO-ESTRN-ANTER
CREMM        WHERE  CSERVC_TARIF       = :GFCTB0D8.CSERVC-TARIF
CREMM      END-EXEC.
CREMM
CREMM      IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
CREMM         (SQLWARN0                EQUAL 'W')
CREMM          INITIALIZE              GFCT0M-ERRO-SQL
CREMM
CREMM          MOVE 'GFCT0212'         TO GFCT0M-PROGRAMA
CREMM          MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
CREMM          MOVE 'SERVC_TARIF_PRINC' TO GFCT0M-NOME-TAB
CREMM          MOVE 'UPDATE'           TO GFCT0M-COMANDO-SQL
CREMM          MOVE  SQLCODE           TO GFCT0M-SQLCODE
CREMM                                     GFCTAY-COD-SQL-ERRO
CREMM          MOVE '0200'             TO GFCT0M-LOCAL
CREMM          MOVE  SQLCA             TO GFCT0M-SQLCA-AREA
CREMM          MOVE  9                 TO GFCTAY-ERRO
CREMM          MOVE  19                TO GFCTAY-COD-MSG-ERRO
CREMM                                     WRK-5000-COD-MSG
CREMM
CREMM          PERFORM 1110-00-OBTER-DESCRICAO-MSG
CREMM
CREMM          STRING WRK-5000-DESC-MSG
CREMM                 ' - GFCTB0D8'
CREMM          DELIMITED BY '  '       INTO GFCTAY-DESC-MSG-ERRO
CREMM
CREMM          INITIALIZE GFCTAY-DADOS
CREMM
CREMM          PERFORM 4000-00-FINALIZAR
CREMM      END-IF.
CREMM
CREMM *----------------------------------------------------------------*
CREMM  2620-00-99-FIM.                  EXIT.
CREMM *----------------------------------------------------------------*
           EJECT
SP0704*----------------------------------------------------------------*
SP0704 2630-00-CANCELAR-VERSOES-ANTER   SECTION.
SP0704*----------------------------------------------------------------*
SP0704
SP0704     MOVE GFCTAX-CSERVC-TARIF    TO CSERVC-TARIF      OF GFCTB087.
SP0704     MOVE GFCTAX-HIDTFD-MANUT-SERVC
SP0704                                 TO HIDTFD-MANUT-SERVC
SP0704                                                      OF GFCTB087.
SP0704
SP0704     EXEC SQL
SP0704       UPDATE DB2PRD.HIST_TARIF_PRINC
SP0704          SET CSIT_LIBRC_TARIF    = 09
SP0704        WHERE CSERVC_TARIF        = :GFCTB087.CSERVC-TARIF
SP0704          AND HIDTFD_MANUT_SERVC <  :GFCTB087.HIDTFD-MANUT-SERVC
SP0704          AND CSIT_LIBRC_TARIF   IN (04, 05, 06)
SP0704     END-EXEC.
SP0704
SP0704     IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
SP0704        (SQLWARN0                EQUAL 'W')
SP0704         INITIALIZE              GFCT0M-ERRO-SQL
SP0704
SP0704         MOVE 'GFCT0212'         TO GFCT0M-PROGRAMA
SP0704         MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
SP0704         MOVE 'HIST_TARIF_PRINC' TO GFCT0M-NOME-TAB
SP0704         MOVE 'UPDATE'           TO GFCT0M-COMANDO-SQL
SP0704         MOVE  SQLCODE           TO GFCT0M-SQLCODE
SP0704                                    GFCTAY-COD-SQL-ERRO
SP0704         MOVE '0210'             TO GFCT0M-LOCAL
SP0704         MOVE  SQLCA             TO GFCT0M-SQLCA-AREA
SP0704         MOVE  9                 TO GFCTAY-ERRO
SP0704         MOVE  19                TO GFCTAY-COD-MSG-ERRO
SP0704                                    WRK-5000-COD-MSG
SP0704
SP0704         PERFORM 1110-00-OBTER-DESCRICAO-MSG
SP0704
SP0704         STRING WRK-5000-DESC-MSG
SP0704                ' - GFCTB087'
SP0704         DELIMITED BY '  '       INTO GFCTAY-DESC-MSG-ERRO
SP0704
SP0704         INITIALIZE GFCTAY-DADOS
SP0704
SP0704         PERFORM 4000-00-FINALIZAR
SP0704     END-IF.
SP0704
SP0704*----------------------------------------------------------------*
SP0704 2630-00-99-FIM.                  EXIT.
SP0704*----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2700-00-PROCESSAR-INCLUSAO       SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-LANCAMENTO        TO CTPO-LCTO-CTBIL    OF GFCTB085

           MOVE GFCTAX-HIDTFD-MANUT-SERVC
                                      TO HIDTFD-MANUT-SERVC OF GFCTB085.
           MOVE GFCTAX-CSEQ-MANUT-SERVC
                                      TO CSEQ-MANUT-SERVC   OF GFCTB085.

           MOVE ZEROS                 TO CEMPR              OF GFCTB085
                                         CGRP-RZ-CTBIL      OF GFCTB085
                                         CSGRP-RZ-CTBIL     OF GFCTB085
                                         CCTA-EVNTO-CTBIL   OF GFCTB085
                                         CDOCTO-CTBIL       OF GFCTB085

           EXEC SQL
             INSERT INTO DB2PRD.HIST_EVNTO_CTBIL
                  (CSERVC_TARIF,
                   HIDTFD_MANUT_SERVC,
                   CSEQ_MANUT_SERVC,
                   CTPO_LCTO_CTBIL,
                   CSEQ_EVNTO_CTBIL,
                   CINDCD_DEPDC_CTBIL,
                   PATRIC_CTBIL,
                   CDEPDC_CTLZO,
                   CPRODT_CTBIL,
                   COPER_CTBIL,
                   CEMPR,
                   CGRP_RZ_CTBIL,
                   CSGRP_RZ_CTBIL ,
                   CCTA_EVNTO_CTBIL,
                   CDOCTO_CTBIL)
               VALUES
                  (:GFCTB085.CSERVC-TARIF,
                   :GFCTB085.HIDTFD-MANUT-SERVC,
                   :GFCTB085.CSEQ-MANUT-SERVC,
                   :GFCTB085.CTPO-LCTO-CTBIL,
                   :GFCTB085.CSEQ-EVNTO-CTBIL,
                   :GFCTB085.CINDCD-DEPDC-CTBIL,
                   :GFCTB085.PATRIC-CTBIL,
                   :GFCTB085.CDEPDC-CTLZO,
                   :GFCTB085.CPRODT-CTBIL,
                   :GFCTB085.COPER-CTBIL,
                   :GFCTB085.CEMPR,
                   :GFCTB085.CGRP-RZ-CTBIL,
                   :GFCTB085.CSGRP-RZ-CTBIL,
                   :GFCTB085.CCTA-EVNTO-CTBIL,
                   :GFCTB085.CDOCTO-CTBIL)
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL

               MOVE 'GFCT0212'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'HIST_EVNTO_CTBIL' TO GFCT0M-NOME-TAB
               MOVE 'INSERT'           TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE           TO GFCT0M-SQLCODE
                                          GFCTAY-COD-SQL-ERRO
               MOVE '0220'             TO GFCT0M-LOCAL
               MOVE  SQLCA             TO GFCT0M-SQLCA-AREA
               MOVE  9                 TO GFCTAY-ERRO
               MOVE  18                TO GFCTAY-COD-MSG-ERRO
                                          WRK-5000-COD-MSG

               PERFORM 1110-00-OBTER-DESCRICAO-MSG

               STRING WRK-5000-DESC-MSG
                      ' - GFCTB085'
               DELIMITED BY '  '       INTO GFCTAY-DESC-MSG-ERRO

               INITIALIZE GFCTAY-DADOS

               PERFORM 4000-00-FINALIZAR
           END-IF.

           PERFORM 2300-00-LER-CURSOR.

      *----------------------------------------------------------------*
       2700-00-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       2800-00-FECHAR-CURSOR            SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             CLOSE CSR-GFCTB085
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL

               MOVE 'GFCT0212'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'HIST_EVNTO_CTBIL' TO GFCT0M-NOME-TAB
               MOVE 'CLOSE'            TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE           TO GFCT0M-SQLCODE
                                          GFCTAY-COD-SQL-ERRO
               MOVE '0230'             TO GFCT0M-LOCAL
               MOVE  SQLCA             TO GFCT0M-SQLCA-AREA
               MOVE  9                 TO GFCTAY-ERRO
               MOVE  11                TO GFCTAY-COD-MSG-ERRO
                                          WRK-5000-COD-MSG

               PERFORM 1110-00-OBTER-DESCRICAO-MSG

               STRING WRK-5000-DESC-MSG
                      ' - GFCTB085'
               DELIMITED BY '  '       INTO GFCTAY-DESC-MSG-ERRO

               INITIALIZE GFCTAY-DADOS

               PERFORM 4000-00-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2800-00-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2900-00-ALTERAR-PARAMETRO-CONT   SECTION.
      *----------------------------------------------------------------*

           PERFORM
            VARYING IND-1               FROM 1 BY 1
              UNTIL IND-1               GREATER 5
                 IF GFCTAX-OPCAO(IND-1) EQUAL 'S'
                    COMPUTE WRK-PATRIC  = WRK-PATRIC
                                          + GFCTAX-PATRIC-CTBIL(IND-1)
               END-IF
           END-PERFORM.

           PERFORM 2110-00-VALIDAR-PERCENTUAL.

           PERFORM
            VARYING IND-1               FROM 1 BY 1
              UNTIL IND-1               GREATER 5
                 IF GFCTAX-OPCAO(IND-1) NOT EQUAL 'S'
                    GO TO 2900-00-99-FIM
                 ELSE
                    PERFORM 2910-00-EFETUAR-ALTERACAO
                 END-IF
           END-PERFORM.

      *----------------------------------------------------------------*
       2900-00-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       2910-00-EFETUAR-ALTERACAO        SECTION.
      *----------------------------------------------------------------*

           IF  GFCTAX-CINDCD-DEPDC-CTBIL(IND-1)
                                       EQUAL 3
               IF  GFCTAX-CJUNC-DEPDC-CTRLZ(IND-1)
                                       EQUAL ZEROS
                   MOVE 1              TO GFCTAY-ERRO
                   MOVE ZEROS          TO GFCTAY-COD-SQL-ERRO
                   MOVE 2              TO GFCTAY-COD-MSG-ERRO
                                          WRK-5000-COD-MSG

                   PERFORM 1110-00-OBTER-DESCRICAO-MSG

                   MOVE WRK-5000-DESC-MSG
                                       TO GFCTAY-DESC-MSG-ERRO

                   INITIALIZE GFCTAY-DADOS

                   PERFORM 4000-00-FINALIZAR
               ELSE
                   PERFORM 2121-00-VALIDAR-DEP-CENTR
               END-IF
           END-IF.

           PERFORM 2122-00-VALIDAR-OPER-TRIC.

           MOVE GFCTAX-CSERVC-TARIF   TO CSERVC-TARIF       OF GFCTB085.
           MOVE GFCTAX-HIDTFD-MANUT-SERVC
                                      TO HIDTFD-MANUT-SERVC OF GFCTB085.
           MOVE GFCTAX-CSEQ-MANUT-SERVC
                                      TO CSEQ-MANUT-SERVC   OF GFCTB085.
           MOVE ZEROS                 TO CPRODT-CTBIL       OF GFCTB085.
           MOVE GFCTAX-COPER-CTBIL(IND-1)
                                      TO COPER-CTBIL        OF GFCTB085.
           MOVE GFCTAX-CTPO-LCTO-CTBIL(IND-1)
                                      TO CTPO-LCTO-CTBIL    OF GFCTB085.
           MOVE GFCTAX-CSEQ-EVNTO-CTBIL(IND-1)
                                      TO CSEQ-EVNTO-CTBIL   OF GFCTB085.
           MOVE GFCTAX-CINDCD-DEPDC-CTBIL(IND-1)
                                      TO CINDCD-DEPDC-CTBIL OF GFCTB085.
           MOVE GFCTAX-PATRIC-CTBIL(IND-1)
                                      TO PATRIC-CTBIL       OF GFCTB085.
           MOVE GFCTAX-CJUNC-DEPDC-CTRLZ(IND-1)
                                      TO CDEPDC-CTLZO       OF GFCTB085.

           PERFORM 2911-00-ATUALIZAR-GFCTB085.

CREMM      IF  GFCTAX-MASTER   EQUAL 'S' AND
CREMM          W-TEM-FUTURA    EQUAL 'S'
CREMM          PERFORM 2912-00-UPDATE-GFCTB047
CREMM      END-IF.

      *----------------------------------------------------------------*
       2910-00-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2911-00-ATUALIZAR-GFCTB085       SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             UPDATE DB2PRD.HIST_EVNTO_CTBIL
               SET  CINDCD_DEPDC_CTBIL = :GFCTB085.CINDCD-DEPDC-CTBIL,
                    PATRIC_CTBIL       = :GFCTB085.PATRIC-CTBIL,
                    CDEPDC_CTLZO       = :GFCTB085.CDEPDC-CTLZO,
                    CPRODT_CTBIL       = :GFCTB085.CPRODT-CTBIL,
                    COPER_CTBIL        = :GFCTB085.COPER-CTBIL
             WHERE  CSERVC_TARIF       = :GFCTB085.CSERVC-TARIF
               AND  HIDTFD_MANUT_SERVC = :GFCTB085.HIDTFD-MANUT-SERVC
               AND  CSEQ_MANUT_SERVC   = :GFCTB085.CSEQ-MANUT-SERVC
               AND  CTPO_LCTO_CTBIL    = :GFCTB085.CTPO-LCTO-CTBIL
               AND  CSEQ_EVNTO_CTBIL   = :GFCTB085.CSEQ-EVNTO-CTBIL
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL

               MOVE 'GFCT0212'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'HIST_EVNTO_CTBIL' TO GFCT0M-NOME-TAB
               MOVE 'UPDATE'           TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE           TO GFCT0M-SQLCODE
                                          GFCTAY-COD-SQL-ERRO
               MOVE '0240'             TO GFCT0M-LOCAL
               MOVE  SQLCA             TO GFCT0M-SQLCA-AREA
               MOVE  9                 TO GFCTAY-ERRO
               MOVE  19                TO GFCTAY-COD-MSG-ERRO
                                          WRK-5000-COD-MSG

               PERFORM 1110-00-OBTER-DESCRICAO-MSG

               STRING WRK-5000-DESC-MSG
                      ' - GFCTB085'
               DELIMITED BY '  '       INTO GFCTAY-DESC-MSG-ERRO

               INITIALIZE GFCTAY-DADOS

               PERFORM 4000-00-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 1                  TO GFCTAY-ERRO
               MOVE SQLCODE            TO GFCTAY-COD-SQL-ERRO
               MOVE 10                 TO GFCTAY-COD-MSG-ERRO
                                          WRK-5000-COD-MSG

               PERFORM 1110-00-OBTER-DESCRICAO-MSG

               MOVE WRK-5000-DESC-MSG  TO GFCTAY-DESC-MSG-ERRO

               INITIALIZE GFCTAY-DADOS

               PERFORM 4000-00-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2911-00-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
           EJECT
CREMM *----------------------------------------------------------------*
CREMM  2912-00-UPDATE-GFCTB047          SECTION.
CREMM *----------------------------------------------------------------*
CREMM
CREMM      MOVE GFCTAX-CSERVC-TARIF   TO CSERVC-TARIF       OF GFCTB047.
CREMM      MOVE ZEROS                 TO CPRODT-CTBIL       OF GFCTB047.
CREMM      MOVE GFCTAX-COPER-CTBIL(IND-1)
CREMM                                 TO COPER-CTBIL        OF GFCTB047.
CREMM      MOVE GFCTAX-CTPO-LCTO-CTBIL(IND-1)
CREMM                                 TO CTPO-LCTO-CTBIL    OF GFCTB047.
CREMM      MOVE GFCTAX-CSEQ-EVNTO-CTBIL(IND-1)
CREMM                                 TO CSEQ-EVNTO-CTBIL   OF GFCTB047.
CREMM      MOVE GFCTAX-CINDCD-DEPDC-CTBIL(IND-1)
CREMM                                 TO CINDCD-DEPDC-CTBIL OF GFCTB047.
CREMM      MOVE GFCTAX-PATRIC-CTBIL(IND-1)
CREMM                                 TO PATRIC-CTBIL       OF GFCTB047.
CREMM      MOVE GFCTAX-CJUNC-DEPDC-CTRLZ(IND-1)
CREMM                                 TO CDEPDC-CTLZO       OF GFCTB047.
CREMM
CREMM      EXEC SQL
CREMM        UPDATE DB2PRD.TARIF_EVNTO_CTBIL
CREMM          SET  CINDCD_DEPDC_CTBIL = :GFCTB047.CINDCD-DEPDC-CTBIL,
CREMM               PATRIC_CTBIL       = :GFCTB047.PATRIC-CTBIL,
CREMM               CDEPDC_CTLZO       = :GFCTB047.CDEPDC-CTLZO,
CREMM               CPRODT_CTBIL       = :GFCTB047.CPRODT-CTBIL,
CREMM               COPER_CTBIL        = :GFCTB047.COPER-CTBIL
CREMM        WHERE  CSERVC_TARIF       = :GFCTB047.CSERVC-TARIF
CREMM          AND  CTPO_LCTO_CTBIL    = :GFCTB047.CTPO-LCTO-CTBIL
CREMM          AND  CSEQ_EVNTO_CTBIL   = :GFCTB047.CSEQ-EVNTO-CTBIL
CREMM      END-EXEC.
CREMM
CREMM      IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
CREMM         (SQLWARN0                EQUAL 'W')
CREMM          INITIALIZE              GFCT0M-ERRO-SQL
CREMM
CREMM          MOVE 'GFCT0212'         TO GFCT0M-PROGRAMA
CREMM          MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
CREMM          MOVE 'TARIF_EVNTO_CTBIL' TO GFCT0M-NOME-TAB
CREMM          MOVE 'UPDATE'           TO GFCT0M-COMANDO-SQL
CREMM          MOVE  SQLCODE           TO GFCT0M-SQLCODE
CREMM                                     GFCTAY-COD-SQL-ERRO
CREMM          MOVE '0250'             TO GFCT0M-LOCAL
CREMM          MOVE  SQLCA             TO GFCT0M-SQLCA-AREA
CREMM          MOVE  9                 TO GFCTAY-ERRO
CREMM          MOVE  19                TO GFCTAY-COD-MSG-ERRO
CREMM                                     WRK-5000-COD-MSG
CREMM
CREMM          PERFORM 1110-00-OBTER-DESCRICAO-MSG
CREMM
CREMM          STRING WRK-5000-DESC-MSG
CREMM                 ' - GFCTB047'
CREMM          DELIMITED BY '  '       INTO GFCTAY-DESC-MSG-ERRO
CREMM
CREMM          INITIALIZE GFCTAY-DADOS
CREMM
CREMM          PERFORM 4000-00-FINALIZAR
CREMM      END-IF.
CREMM
CREMM *----------------------------------------------------------------*
CREMM  2912-00-99-FIM.                  EXIT.
CREMM *----------------------------------------------------------------*
CREMM      EJECT
      *----------------------------------------------------------------*
       2921-00-EXCLUIR-PARAMETRO-CONT   SECTION.
      *----------------------------------------------------------------*

           PERFORM
            VARYING IND-1               FROM 1 BY 1
              UNTIL IND-1               GREATER 5
                 IF GFCTAX-OPCAO(IND-1) EQUAL 'S'
                    COMPUTE WRK-PATRIC  = WRK-PATRIC
                                        + GFCTAX-PATRIC-CTBIL(IND-1)
               END-IF
           END-PERFORM.

           PERFORM 2110-00-VALIDAR-PERCENTUAL.

           PERFORM
            VARYING IND-1               FROM 1 BY 1
              UNTIL IND-1               GREATER 5
                 IF GFCTAX-OPCAO(IND-1) NOT EQUAL 'S'
                    GO TO 2921-00-99-FIM
                 ELSE
                    MOVE GFCTAX-CSERVC-TARIF
                                      TO CSERVC-TARIF       OF GFCTB085
                    MOVE GFCTAX-HIDTFD-MANUT-SERVC
                                      TO HIDTFD-MANUT-SERVC OF GFCTB085
                    MOVE GFCTAX-CSEQ-MANUT-SERVC
                                      TO CSEQ-MANUT-SERVC   OF GFCTB085
                    MOVE GFCTAX-CTPO-LCTO-CTBIL(IND-1)
                                      TO CTPO-LCTO-CTBIL    OF GFCTB085
                    MOVE GFCTAX-CSEQ-EVNTO-CTBIL(IND-1)
                                      TO CSEQ-EVNTO-CTBIL   OF GFCTB085
                    PERFORM 2921-10-DELETAR-GFCTB085

CREMM               IF  GFCTAX-MASTER   EQUAL 'S' AND
CREMM                   W-TEM-FUTURA    EQUAL 'S'
CREMM                   PERFORM 2921-20-DELETAR-GFCTB047
CREMM               END-IF
                 END-IF

           END-PERFORM.

      *----------------------------------------------------------------*
       2921-00-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2921-10-DELETAR-GFCTB085         SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             DELETE FROM DB2PRD.HIST_EVNTO_CTBIL
             WHERE  CSERVC_TARIF       = :GFCTB085.CSERVC-TARIF
               AND  HIDTFD_MANUT_SERVC = :GFCTB085.HIDTFD-MANUT-SERVC
               AND  CSEQ_MANUT_SERVC   = :GFCTB085.CSEQ-MANUT-SERVC
               AND  CTPO_LCTO_CTBIL    = :GFCTB085.CTPO-LCTO-CTBIL
               AND  CSEQ_EVNTO_CTBIL   = :GFCTB085.CSEQ-EVNTO-CTBIL
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL

               MOVE 'GFCT0212'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'HIST_EVNTO_CTBIL' TO GFCT0M-NOME-TAB
               MOVE 'DELETE'           TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE           TO GFCT0M-SQLCODE
                                          GFCTAY-COD-SQL-ERRO
               MOVE '0260'             TO GFCT0M-LOCAL
               MOVE  SQLCA             TO GFCT0M-SQLCA-AREA
               MOVE  9                 TO GFCTAY-ERRO
               MOVE  20                TO GFCTAY-COD-MSG-ERRO
                                          WRK-5000-COD-MSG

               PERFORM 1110-00-OBTER-DESCRICAO-MSG

               STRING WRK-5000-DESC-MSG
                      ' - GFCTB085'
               DELIMITED BY '  '       INTO GFCTAY-DESC-MSG-ERRO

               INITIALIZE GFCTAY-DADOS

               PERFORM 4000-00-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 1                  TO GFCTAY-ERRO
               MOVE SQLCODE            TO GFCTAY-COD-SQL-ERRO
               MOVE 10                 TO GFCTAY-COD-MSG-ERRO
                                          WRK-5000-COD-MSG

               PERFORM 1110-00-OBTER-DESCRICAO-MSG

               MOVE WRK-5000-DESC-MSG  TO GFCTAY-DESC-MSG-ERRO

               INITIALIZE GFCTAY-DADOS

               PERFORM 4000-00-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       2921-10-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

CREMM *----------------------------------------------------------------*
CREMM  2921-20-DELETAR-GFCTB047         SECTION.
CREMM *----------------------------------------------------------------*
CREMM
CREMM       MOVE GFCTAX-CSERVC-TARIF  TO CSERVC-TARIF       OF GFCTB047
CREMM       MOVE GFCTAX-CTPO-LCTO-CTBIL(IND-1)
CREMM                                 TO CTPO-LCTO-CTBIL    OF GFCTB047
CREMM       MOVE GFCTAX-CSEQ-EVNTO-CTBIL(IND-1)
CREMM                                 TO CSEQ-EVNTO-CTBIL   OF GFCTB047
CREMM      EXEC SQL
CREMM        DELETE FROM DB2PRD.TARIF_EVNTO_CTBIL
CREMM        WHERE  CSERVC_TARIF       = :GFCTB047.CSERVC-TARIF
CREMM          AND  CTPO_LCTO_CTBIL    = :GFCTB047.CTPO-LCTO-CTBIL
CREMM          AND  CSEQ_EVNTO_CTBIL   = :GFCTB047.CSEQ-EVNTO-CTBIL
CREMM      END-EXEC.
CREMM
CREMM      IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
CREMM         (SQLWARN0                EQUAL 'W')
CREMM          INITIALIZE              GFCT0M-ERRO-SQL
CREMM
CREMM          MOVE 'GFCT0212'         TO GFCT0M-PROGRAMA
CREMM          MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
CREMM          MOVE 'TARIF_EVNTO_CTBIL' TO GFCT0M-NOME-TAB
CREMM          MOVE 'DELETE'           TO GFCT0M-COMANDO-SQL
CREMM          MOVE  SQLCODE           TO GFCT0M-SQLCODE
CREMM                                     GFCTAY-COD-SQL-ERRO
CREMM          MOVE '0270'             TO GFCT0M-LOCAL
CREMM          MOVE  SQLCA             TO GFCT0M-SQLCA-AREA
CREMM          MOVE  9                 TO GFCTAY-ERRO
CREMM          MOVE  20                TO GFCTAY-COD-MSG-ERRO
CREMM                                     WRK-5000-COD-MSG
CREMM
CREMM          PERFORM 1110-00-OBTER-DESCRICAO-MSG
CREMM
CREMM          STRING WRK-5000-DESC-MSG
CREMM                 ' - GFCTB047'
CREMM          DELIMITED BY '  '       INTO GFCTAY-DESC-MSG-ERRO
CREMM
CREMM          INITIALIZE GFCTAY-DADOS
CREMM
CREMM          PERFORM 4000-00-FINALIZAR
CREMM      END-IF.
CREMM
CREMM *----------------------------------------------------------------*
CREMM  2921-20-99-FIM.                  EXIT.
CREMM *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3000-00-ABRIR-CSR-GFCTB084       SECTION.
      *----------------------------------------------------------------*

           MOVE 'N'                    TO WRK-FIM-CSR1.
           MOVE GFCTAX-CSERVC-TARIF    TO CSERVC-TARIF OF GFCTB084.

           COMPUTE CSEQ-MANUT-SERVC    OF GFCTB084
                                       = GFCTAX-CSEQ-MANUT-SERVC - 1.

           MOVE GFCTAX-DINIC-VGCIA-TARIF
                                       TO DFIM-VGCIA-COMPS OF GFCTB084.

           EXEC SQL
               OPEN CSR-GFCTB084
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL

               MOVE 'GFCT0212'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'HIST_COMPS_PCOTE' TO GFCT0M-NOME-TAB
               MOVE 'OPEN'             TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
               MOVE '0280'             TO GFCT0M-LOCAL
               MOVE SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE 9                  TO GFCTAY-ERRO
               MOVE SQLCODE            TO GFCTAY-COD-SQL-ERRO
               MOVE 0005               TO GFCTAY-COD-MSG-ERRO
                                          WRK-5000-COD-MSG

               PERFORM 1110-00-OBTER-DESCRICAO-MSG

               STRING WRK-5000-DESC-MSG
                      ' - GFCTB084'
               DELIMITED BY '  '       INTO GFCTAY-DESC-MSG-ERRO

               INITIALIZE GFCTAY-DADOS

               PERFORM 4000-00-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       3000-00-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3100-00-LER-CSR-GFCTB084         SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             FETCH CSR-GFCTB084 INTO
                   :GFCTB084.CSERVC-TARIF,
                   :GFCTB084.HIDTFD-MANUT-SERVC,
                   :GFCTB084.CSEQ-MANUT-SERVC,
                   :GFCTB084.CSEQ-COMPO-PCOTE,
                   :GFCTB084.DINIC-VGCIA-COMPS,
                   :GFCTB084.DFIM-VGCIA-COMPS,
                   :GFCTB084.CTARIF-COMPO-PCOTE,
                   :GFCTB084.CGRP-SERVC,
                   :GFCTB084.QFRANQ-COMPO-PCOTE,
                   :GFCTB084.CPER-COBR-COMPO,
                   :GFCTB084.VEXCED-COMPO-PCOTE,
                   :GFCTB084.PEXCED-COMPO-PCOTE,
                   :GFCTB084.QDIA-COBR-COMPO,
                   :GFCTB084.RSEGDA-LIN-EXCED,
161110             :GFCTB084.DDIA-COBR-TARIF,
161110             :GFCTB084.CIDTFD-COBR-FERI,
161110             :GFCTB084.CIDTFD-VLR-EXCED,
161110             :GFCTB084.CIDTFD-CTRL-UTILZ,
161110             :GFCTB084.CINDCD-ISENC-UTILZ
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL

               MOVE 'GFCT0212'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'HIST_COMPS_PCOTE' TO GFCT0M-NOME-TAB
               MOVE 'FETCH'            TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
               MOVE '0290'             TO GFCT0M-LOCAL
               MOVE SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE 9                  TO GFCTAY-ERRO
               MOVE SQLCODE            TO GFCTAY-COD-SQL-ERRO
               MOVE 0006               TO GFCTAY-COD-MSG-ERRO
                                          WRK-5000-COD-MSG

               PERFORM 1110-00-OBTER-DESCRICAO-MSG

               STRING WRK-5000-DESC-MSG
                      ' - GFCTB084'
               DELIMITED BY '  '       INTO GFCTAY-DESC-MSG-ERRO

               INITIALIZE GFCTAY-DADOS

               PERFORM 4000-00-FINALIZAR
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FIM-CSR1
010211     ELSE
010211         IF  CINDCD-ISENC-UTILZ OF GFCTB084
010211                                 NOT EQUAL 'N' AND
010211             CINDCD-ISENC-UTILZ OF GFCTB084
010211                                 NOT EQUAL 'S'
010211             MOVE 'N'            TO CINDCD-ISENC-UTILZ
010211                                                      OF GFCTB084
010211         END-IF
           END-IF.

      *----------------------------------------------------------------*
       3100-00-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3200-00-PROCESSAR-INCLUSAO       SECTION.
      *----------------------------------------------------------------*

           MOVE GFCTAX-HIDTFD-MANUT-SERVC
                                      TO HIDTFD-MANUT-SERVC OF GFCTB084.
           MOVE GFCTAX-CSEQ-MANUT-SERVC
                                      TO CSEQ-MANUT-SERVC   OF GFCTB084.

           MOVE GFCTAX-DINIC-VGCIA-TARIF
                                      TO WRK-DATA-DB2.

           MOVE WRK-DIA-DB2           TO WRK-DIA-INI-VIG-TARIF.
           MOVE WRK-MES-DB2           TO WRK-MES-INI-VIG-TARIF.
           MOVE WRK-ANO-DB2           TO WRK-ANO-INI-VIG-TARIF.

           MOVE DINIC-VGCIA-COMPS     OF GFCTB084
                                      TO WRK-DATA-DB2.

           MOVE WRK-DIA-DB2           TO WRK-DIA-INI-VIG-COMPS.
           MOVE WRK-MES-DB2           TO WRK-MES-INI-VIG-COMPS.
           MOVE WRK-ANO-DB2           TO WRK-ANO-INI-VIG-COMPS.

           IF WRK-DATA-INI-VIG-COMPS  LESS WRK-DATA-INI-VIG-TARIF
              MOVE GFCTAX-DINIC-VGCIA-TARIF
                                      TO DINIC-VGCIA-COMPS OF GFCTB084
           END-IF.

           EXEC SQL
             INSERT INTO DB2PRD.HIST_COMPS_PCOTE
                  (CSERVC_TARIF,
                   HIDTFD_MANUT_SERVC,
                   CSEQ_MANUT_SERVC,
                   CSEQ_COMPO_PCOTE,
                   DINIC_VGCIA_COMPS,
                   DFIM_VGCIA_COMPS,
                   CGRP_SERVC,
                   CTARIF_COMPO_PCOTE,
                   QFRANQ_COMPO_PCOTE,
                   CPER_COBR_COMPO,
                   VEXCED_COMPO_PCOTE,
                   PEXCED_COMPO_PCOTE,
                   QDIA_COBR_COMPO,
                   RSEGDA_LIN_EXCED,
161110             DDIA_COBR_TARIF,
161110             CIDTFD_COBR_FERI,
161110             CIDTFD_VLR_EXCED,
161110             CIDTFD_CTRL_UTILZ,
161110             CINDCD_ISENC_UTILZ)
               VALUES
                  (:GFCTB084.CSERVC-TARIF,
                   :GFCTB084.HIDTFD-MANUT-SERVC,
                   :GFCTB084.CSEQ-MANUT-SERVC,
                   :GFCTB084.CSEQ-COMPO-PCOTE,
                   :GFCTB084.DINIC-VGCIA-COMPS,
                   :GFCTB084.DFIM-VGCIA-COMPS,
                   :GFCTB084.CGRP-SERVC,
                   :GFCTB084.CTARIF-COMPO-PCOTE,
                   :GFCTB084.QFRANQ-COMPO-PCOTE,
                   :GFCTB084.CPER-COBR-COMPO,
                   :GFCTB084.VEXCED-COMPO-PCOTE,
                   :GFCTB084.PEXCED-COMPO-PCOTE,
                   :GFCTB084.QDIA-COBR-COMPO,
                   :GFCTB084.RSEGDA-LIN-EXCED,
161110             :GFCTB084.DDIA-COBR-TARIF,
161110             :GFCTB084.CIDTFD-COBR-FERI,
161110             :GFCTB084.CIDTFD-VLR-EXCED,
161110             :GFCTB084.CIDTFD-CTRL-UTILZ,
161110             :GFCTB084.CINDCD-ISENC-UTILZ)
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL

               MOVE 'GFCT0212'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'HIST_COMPS_PCOTE' TO GFCT0M-NOME-TAB
               MOVE 'INSERT'           TO GFCT0M-COMANDO-SQL
               MOVE  SQLCODE           TO GFCT0M-SQLCODE
                                          GFCTAY-COD-SQL-ERRO
               MOVE '0300'             TO GFCT0M-LOCAL
               MOVE  SQLCA             TO GFCT0M-SQLCA-AREA
               MOVE  9                 TO GFCTAY-ERRO
               MOVE  18                TO GFCTAY-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               PERFORM 1110-00-OBTER-DESCRICAO-MSG

               STRING WRK-5000-DESC-MSG
               ' - GFCTB085'
               DELIMITED BY '  '       INTO GFCTAY-DESC-MSG-ERRO
               INITIALIZE GFCTAY-DADOS
               PERFORM 4000-00-FINALIZAR
           END-IF.

           PERFORM 3100-00-LER-CSR-GFCTB084.

      *----------------------------------------------------------------*
       3200-00-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3300-00-FECHAR-CSR-GFCTB084      SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
                CLOSE CSR-GFCTB084
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS) OR
              (SQLWARN0                EQUAL 'W')
               INITIALIZE              GFCT0M-ERRO-SQL
               MOVE 'GFCT0212'         TO GFCT0M-PROGRAMA
               MOVE 'DB2'              TO GFCT0M-TIPO-ACESSO
               MOVE 'HIST_COMPS_PCOTE' TO GFCT0M-NOME-TAB
               MOVE 'CLOSE'            TO GFCT0M-COMANDO-SQL
               MOVE SQLCODE            TO GFCT0M-SQLCODE
               MOVE '0310'             TO GFCT0M-LOCAL
               MOVE SQLCA              TO GFCT0M-SQLCA-AREA
               MOVE 9                  TO GFCTAY-ERRO
               MOVE SQLCODE            TO GFCTAY-COD-SQL-ERRO
               MOVE 0011               TO GFCTAY-COD-MSG-ERRO
                                          WRK-5000-COD-MSG
               PERFORM 1110-00-OBTER-DESCRICAO-MSG
               STRING WRK-5000-DESC-MSG
               ' - GFCTB084'
               DELIMITED BY '  '       INTO GFCTAY-DESC-MSG-ERRO
               INITIALIZE GFCTAY-DADOS
               PERFORM 4000-00-FINALIZAR
           END-IF.

      *----------------------------------------------------------------*
       3300-00-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4000-00-FINALIZAR                SECTION.
      *----------------------------------------------------------------*

           GOBACK.

      *----------------------------------------------------------------*
       4000-00-99-FIM.                  EXIT.
      *----------------------------------------------------------------*
