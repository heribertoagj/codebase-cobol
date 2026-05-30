      *================================================================*
       IDENTIFICATION DIVISION.
      *================================================================*
       PROGRAM-ID.    RUEC1450.
       AUTHOR.        RAFAEL GONCALVES.

      *================================================================*
      *                     S O N D A  I T                             *
      *----------------------------------------------------------------*
      *                                                                *
      *    PROGRAMADOR    : RAFAEL GONCALVES   - SONDA                 *
      *    ANALISTA       : EDUARDO WATANABE   - SONDA                 *
      *    DATA           : NOVEMBRO/2011                              *
      *                                                                *
      *    OBJETIVO      : SELECIONAR CONTRATOS E PARCELAS DAS         *
      *                    TABELAS RUECB008, RUECB009, RUECV010,       *
      *                    RUECV011, RUECV012, RUECB014, RUECB017,     *
      *                    RUECV018, RUECV019, RUECV022, RUECV023,     *
      *                    RUECB024, RUECV025, RUECV026, RUECB027,     *
      *                    RUECB028, RUECB033, RUECB034, RUECB035,     *
      *                    RUECB036, RUECB038, RUECB040, RUECB041,     *
      *                    RUECB044, RUECB052, RUECB053, RUECB054,     *
      *                    RUECB064, RUECB065, RUECB066, RUECB073,     *
      *                    RUECB074, RUECB075 E RUECB096 ATRAVES DE    *
      *                    CONTRATOS SELECIONADOS POR AGENCIAS.        *
      *                                                                *
      *    ARQUIVOS                                                    *
      *      E/S DDNAME   LAYOUT    DESCRICAO                          *
      *       E  ARQDATAS I#RURACR  ARQUIVO DE DATAS DO RURAL.         *
      *       E  SELCDULA           ARQUIVO DE CONTRATOS SELECIONADOS  *
      *                             NO PROGRAMA RUEC1410.              *
      *       S  RUECB008           ARQUIVO DE REGISTROS SELECIONADOS. *
      *       S  RUECB009           ARQUIVO DE REGISTROS SELECIONADOS. *
      *       S  RUECV010           ARQUIVO DE REGISTROS SELECIONADOS. *
      *       S  RUECV011           ARQUIVO DE REGISTROS SELECIONADOS. *
      *       S  RUECV012           ARQUIVO DE REGISTROS SELECIONADOS. *
      *       S  RUECB014           ARQUIVO DE REGISTROS SELECIONADOS. *
      *       S  RUECB017           ARQUIVO DE REGISTROS SELECIONADOS. *
      *       S  RUECV018           ARQUIVO DE REGISTROS SELECIONADOS. *
      *       S  RUECV019           ARQUIVO DE REGISTROS SELECIONADOS. *
      *       S  RUECV022           ARQUIVO DE REGISTROS SELECIONADOS. *
      *       S  RUECV023           ARQUIVO DE REGISTROS SELECIONADOS. *
      *       S  RUECB024           ARQUIVO DE REGISTROS SELECIONADOS. *
      *       S  RUECV025           ARQUIVO DE REGISTROS SELECIONADOS. *
      *       S  RUECV026           ARQUIVO DE REGISTROS SELECIONADOS. *
      *       S  RUECB027           ARQUIVO DE REGISTROS SELECIONADOS. *
      *       S  RUECB028           ARQUIVO DE REGISTROS SELECIONADOS. *
      *       S  RUECB033           ARQUIVO DE REGISTROS SELECIONADOS. *
      *       S  RUECB034           ARQUIVO DE REGISTROS SELECIONADOS. *
      *       S  RUECB035           ARQUIVO DE REGISTROS SELECIONADOS. *
      *       S  RUECB036           ARQUIVO DE REGISTROS SELECIONADOS. *
      *       S  RUECB038           ARQUIVO DE REGISTROS SELECIONADOS. *
      *       S  RUECB040           ARQUIVO DE REGISTROS SELECIONADOS. *
      *       S  RUECB041           ARQUIVO DE REGISTROS SELECIONADOS. *
      *       S  RUECB044           ARQUIVO DE REGISTROS SELECIONADOS. *
      *       S  RUECB052           ARQUIVO DE REGISTROS SELECIONADOS. *
      *       S  RUECB053           ARQUIVO DE REGISTROS SELECIONADOS. *
      *       S  RUECB054           ARQUIVO DE REGISTROS SELECIONADOS. *
      *       S  RUECB064           ARQUIVO DE REGISTROS SELECIONADOS. *
      *       S  RUECB065           ARQUIVO DE REGISTROS SELECIONADOS. *
      *       S  RUECB066           ARQUIVO DE REGISTROS SELECIONADOS. *
      *       S  RUECB073           ARQUIVO DE REGISTROS SELECIONADOS. *
      *       S  RUECB074           ARQUIVO DE REGISTROS SELECIONADOS. *
      *       S  RUECB075           ARQUIVO DE REGISTROS SELECIONADOS. *
      *       S  RUECB096           ARQUIVO DE REGISTROS SELECIONADOS. *
AC0613*       S  RUECB097           ARQUIVO DE REGISTROS SELECIONADOS. *
AC0613*       S  RUECB099           ARQUIVO DE REGISTROS SELECIONADOS. *
AC0613*       S  RUECB09A           ARQUIVO DE REGISTROS SELECIONADOS. *
AC0613*       S  RUECB09B           ARQUIVO DE REGISTROS SELECIONADOS. *
AC0613*       S  RUECB09D           ARQUIVO DE REGISTROS SELECIONADOS. *
AC0613*       S  RUECB09E           ARQUIVO DE REGISTROS SELECIONADOS. *
AC0613*       S  RUECB09F           ARQUIVO DE REGISTROS SELECIONADOS. *
AC0613*       S  RUECB09G           ARQUIVO DE REGISTROS SELECIONADOS. *
AC0613*       S  RUECB09H           ARQUIVO DE REGISTROS SELECIONADOS. *
AC0613*       S  RUECB09I           ARQUIVO DE REGISTROS SELECIONADOS. *
      *                                                                *
      *    TABELAS DB2                                                 *
      *      DCLGEN   TABLENAME                                        *
      *      RUECB008 DB2PRD.PSSOA_CDULA_TEMPR                         *
      *      RUECB009 DB2PRD.GARNT_CDULA_TEMPR                         *
      *      RUECV010 DB2PRD.V01EVNTO_CDULA                            *
      *      RUECV011 DB2PRD.V01RENDA_CDULA                            *
      *      RUECV012 DB2PRD.V01EVNTO_RURAL                            *
      *      RUECB014 DB2PRD.PESSOA_CONTR_RURAL                        *
      *      RUECB017 DB2PRD.HIPOT_CONTR_RURAL                         *
      *      RUECV018 DB2PRD.V01INCON_CONTR                            *
      *      RUECV019 DB2PRD.V01INCON_CDULA                            *
      *      RUECV022 DB2PRD.V01ORCAM_RURAL                            *
      *      RUECV023 DB2PRD.V01ISUMO_ORCAM                            *
      *      RUECB024 DB2PRD.OBS_ORCAM_RURAL                           *
      *      RUECV025 DB2PRD.V01ORCAM_CDULA                            *
      *      RUECV026 DB2PRD.V01ISUMO_CDULA                            *
      *      RUECB027 DB2PRD.OBS_ORCAM_CDULA                           *
      *      RUECB028 DB2PRD.ARZEM_PRODT_RURAL                         *
      *      RUECB033 DB2PRD.CTRL_HIST_IMPRE                           *
      *      RUECB034 DB2PRD.HIST_IMPRE_CDULA                          *
      *      RUECB035 DB2PRD.HIPOTECA_TEMPR                            *
      *      RUECB036 DB2PRD.CONFR_HIPOT_TEMPR                         *
      *      RUECB038 DB2PRD.CONFR_HIPOT_RURAL                         *
      *      RUECB040 DB2PRD.MATR_CONTR_RURAL                          *
      *      RUECB041 DB2PRD.MATR_CDULA_RURAL                          *
      *      RUECB044 DB2PRD.CTDAO_BNDES                               *
      *      RUECB052 DB2PRD.THIST_IMOV_RURAL                          *
      *      RUECB053 DB2PRD.THIST_RBNHO_CONTR                         *
      *      RUECB054 DB2PRD.THIST_RENDA_AGROP                         *
      *      RUECB064 DB2PRD.THIST_DEVLC_CONTR                         *
      *      RUECB065 DB2PRD.TCARTR_CONTR_RURAL                        *
      *      RUECB066 DB2PRD.TOBS_LICEN_AMBTL                          *
      *      RUECB073 DB2PRD.TUTILZ_LIM_OPER                           *
      *      RUECB074 DB2PRD.TINFO_CONJG_CDULA                         *
      *      RUECB075 DB2PRD.TINFO_CONJG_CONT                          *
      *      RUECB096 DB2PRD.THIST_SIT_CDULA                           *
AC0613*      RUECB097 DB2PRD.TCDULA_COMCZ                              *
AC0613*      RUECB099 DB2PRD.TCONTR_ENVIO_RECOR                        *
AC0613*      RUECB09A DB2PRD.TENVIO_RECOR_TEMPR                        *
AC0613*      RUECB09B DB2PRD.THIST_ENVIO_RECOR                         *
AC0613*      RUECB09D DB2PRD.TPCELA_ENVIO_RECOR                        *
AC0613*      RUECB09E DB2PRD.TPSSOA_ENVIO_RECOR                        *
AC0613*      RUECB09F DB2PRD.TPRODT_RURAL_RECOR                        *
AC0613*      RUECB09G DB2PRD.TPRODT_RURAL_CSCDO                        *
AC0613*      RUECB09H DB2PRD.TPRODT_RURAL_TEMPR                        *
AC0613*      RUECB09I DB2PRD.TLASTR_CONTR_RURAL                        *
      *                                                                *
      *    POOLS/MODULOS CHAMADOS                                      *
      *      BRAD0160 - OBTEM NOME DO JOB                              *
      *      BRAD7100 - TRATAMENTO DE ERROS                            *
      *      BRAD7600 - OBTEM DATA/HORA DO SISTEMA                     *
      *                                                                *
      *================================================================*
      *================================================================*
      *                     S O N D A  I T                             *
      *----------------------------------------------------------------*
      *    PROGRAMADOR    : CELSO CAMILLO      - BRQ                   *
      *    ANALISTA       : CELSO CAMILLO      - BRQ                   *
      *    DATA           : JANEIRO/2018                               *
      *                                                                *
      *    OBJETIVO      : INCLUIR CAMPO DATA FIM NA TABELA RUECV022   *
      ******************************************************************
      *================================================================*
       ENVIRONMENT DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       CONFIGURATION                   SECTION.
      *----------------------------------------------------------------*

       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

      *----------------------------------------------------------------*
       INPUT-OUTPUT                    SECTION.
      *----------------------------------------------------------------*
       FILE-CONTROL.

           SELECT  ARQDATAS ASSIGN     TO UT-S-ARQDATAS
                       FILE STATUS     IS WRK-FS-ARQDATAS.

           SELECT  SELCDULA ASSIGN     TO UT-S-SELCDULA
                       FILE STATUS     IS WRK-FS-SELCDULA.

           SELECT  RUEC008S ASSIGN     TO UT-S-RUEC008S
                       FILE STATUS     IS WRK-FS-RUEC008S.

           SELECT  RUEC009S ASSIGN     TO UT-S-RUEC009S
                       FILE STATUS     IS WRK-FS-RUEC009S.

           SELECT  RUEC010S ASSIGN     TO UT-S-RUEC010S
                       FILE STATUS     IS WRK-FS-RUEC010S.

           SELECT  RUEC011S ASSIGN     TO UT-S-RUEC011S
                       FILE STATUS     IS WRK-FS-RUEC011S.

           SELECT  RUEC012S ASSIGN     TO UT-S-RUEC012S
                       FILE STATUS     IS WRK-FS-RUEC012S.

           SELECT  RUEC014S ASSIGN     TO UT-S-RUEC014S
                       FILE STATUS     IS WRK-FS-RUEC014S.

           SELECT  RUEC017S ASSIGN     TO UT-S-RUEC017S
                       FILE STATUS     IS WRK-FS-RUEC017S.

           SELECT  RUEC018S ASSIGN     TO UT-S-RUEC018S
                       FILE STATUS     IS WRK-FS-RUEC018S.

           SELECT  RUEC019S ASSIGN     TO UT-S-RUEC019S
                       FILE STATUS     IS WRK-FS-RUEC019S.

           SELECT  RUEC022S ASSIGN     TO UT-S-RUEC022S
                       FILE STATUS     IS WRK-FS-RUEC022S.

           SELECT  RUEC023S ASSIGN     TO UT-S-RUEC023S
                       FILE STATUS     IS WRK-FS-RUEC023S.

           SELECT  RUEC024S ASSIGN     TO UT-S-RUEC024S
                       FILE STATUS     IS WRK-FS-RUEC024S.

           SELECT  RUEC025S ASSIGN     TO UT-S-RUEC025S
                       FILE STATUS     IS WRK-FS-RUEC025S.

           SELECT  RUEC026S ASSIGN     TO UT-S-RUEC026S
                       FILE STATUS     IS WRK-FS-RUEC026S.

           SELECT  RUEC027S ASSIGN     TO UT-S-RUEC027S
                       FILE STATUS     IS WRK-FS-RUEC027S.

           SELECT  RUEC028S ASSIGN     TO UT-S-RUEC028S
                       FILE STATUS     IS WRK-FS-RUEC028S.

           SELECT  RUEC033S ASSIGN     TO UT-S-RUEC033S
                       FILE STATUS     IS WRK-FS-RUEC033S.

           SELECT  RUEC034S ASSIGN     TO UT-S-RUEC034S
                       FILE STATUS     IS WRK-FS-RUEC034S.

           SELECT  RUEC035S ASSIGN     TO UT-S-RUEC035S
                       FILE STATUS     IS WRK-FS-RUEC035S.

           SELECT  RUEC036S ASSIGN     TO UT-S-RUEC036S
                       FILE STATUS     IS WRK-FS-RUEC036S.

           SELECT  RUEC038S ASSIGN     TO UT-S-RUEC038S
                       FILE STATUS     IS WRK-FS-RUEC038S.

           SELECT  RUEC040S ASSIGN     TO UT-S-RUEC040S
                       FILE STATUS     IS WRK-FS-RUEC040S.

           SELECT  RUEC041S ASSIGN     TO UT-S-RUEC041S
                       FILE STATUS     IS WRK-FS-RUEC041S.

           SELECT  RUEC044S ASSIGN     TO UT-S-RUEC044S
                       FILE STATUS     IS WRK-FS-RUEC044S.

           SELECT  RUEC052S ASSIGN     TO UT-S-RUEC052S
                       FILE STATUS     IS WRK-FS-RUEC052S.

           SELECT  RUEC053S ASSIGN     TO UT-S-RUEC053S
                       FILE STATUS     IS WRK-FS-RUEC053S.

           SELECT  RUEC054S ASSIGN     TO UT-S-RUEC054S
                       FILE STATUS     IS WRK-FS-RUEC054S.

           SELECT  RUEC064S ASSIGN     TO UT-S-RUEC064S
                       FILE STATUS     IS WRK-FS-RUEC064S.

           SELECT  RUEC065S ASSIGN     TO UT-S-RUEC065S
                       FILE STATUS     IS WRK-FS-RUEC065S.

           SELECT  RUEC066S ASSIGN     TO UT-S-RUEC066S
                       FILE STATUS     IS WRK-FS-RUEC066S.

           SELECT  RUEC073S ASSIGN     TO UT-S-RUEC073S
                       FILE STATUS     IS WRK-FS-RUEC073S.

           SELECT  RUEC074S ASSIGN     TO UT-S-RUEC074S
                       FILE STATUS     IS WRK-FS-RUEC074S.

           SELECT  RUEC075S ASSIGN     TO UT-S-RUEC075S
                       FILE STATUS     IS WRK-FS-RUEC075S.

           SELECT  RUEC096S ASSIGN     TO UT-S-RUEC096S
                       FILE STATUS     IS WRK-FS-RUEC096S.

AC0613     SELECT  RUEC097S ASSIGN     TO UT-S-RUEC097S
AC0613                 FILE STATUS     IS WRK-FS-RUEC097S.
AC0613
AC0613     SELECT  RUEC099S ASSIGN     TO UT-S-RUEC099S
AC0613                 FILE STATUS     IS WRK-FS-RUEC099S.
AC0613
AC0613     SELECT  RUEC09AS ASSIGN     TO UT-S-RUEC09AS
AC0613                 FILE STATUS     IS WRK-FS-RUEC09AS.
AC0613
AC0613     SELECT  RUEC09BS ASSIGN     TO UT-S-RUEC09BS
AC0613                 FILE STATUS     IS WRK-FS-RUEC09BS.
AC0613
AC0613     SELECT  RUEC09DS ASSIGN     TO UT-S-RUEC09DS
AC0613                 FILE STATUS     IS WRK-FS-RUEC09DS.
AC0613
AC0613     SELECT  RUEC09ES ASSIGN     TO UT-S-RUEC09ES
AC0613                 FILE STATUS     IS WRK-FS-RUEC09ES.
AC0613
AC0613     SELECT  RUEC09FS ASSIGN     TO UT-S-RUEC09FS
AC0613                 FILE STATUS     IS WRK-FS-RUEC09FS.
AC0613
AC0613     SELECT  RUEC09GS ASSIGN     TO UT-S-RUEC09GS
AC0613                 FILE STATUS     IS WRK-FS-RUEC09GS.
AC0613
AC0613     SELECT  RUEC09HS ASSIGN     TO UT-S-RUEC09HS
AC0613                 FILE STATUS     IS WRK-FS-RUEC09HS.
AC0613
AC0613     SELECT  RUEC09IS ASSIGN     TO UT-S-RUEC09IS
AC0613                 FILE STATUS     IS WRK-FS-RUEC09IS.

      *================================================================*
       DATA DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *    INPUT:  ARQUIVO DE DATAS                                    *
      *            ORG. SEQUENCIAL   -   LRECL = 210                   *
      *----------------------------------------------------------------*

       FD  ARQDATAS
           RECORDING F
           LABEL RECORD STANDARD
           BLOCK 0.

       01  FD-REG-ARQDATAS             PIC  X(210).

      *----------------------------------------------------------------*
      *            ARQUIVO DE CONTRATOS SELECIONADOS                   *
      *            ORG. SEQUENCIAL   -   LRECL = 437                   *
      *----------------------------------------------------------------*

       FD  SELCDULA
           LABEL RECORD STANDARD
           BLOCK 0
           RECORDING F.
       01  FD-REG-SELCDULA             PIC  X(437).

      *----------------------------------------------------------------*
      *    OUTPUT: ARQUIVO DE CONTRATOS SELECIONADAS NO RUECB008       *
      *            ORG. SEQUENCIAL   -   LRECL = 355                   *
      *----------------------------------------------------------------*

       FD  RUEC008S
           LABEL RECORD STANDARD
           BLOCK 0
           RECORDING F.

       01  FD-REG-RUEC008S             PIC  X(355).

      *----------------------------------------------------------------*
      *    OUTPUT: ARQUIVO DE CONTRATOS SELECIONADAS NO RUECB009       *
      *            ORG. SEQUENCIAL   -   LRECL = 530                   *
      *----------------------------------------------------------------*

        FD  RUEC009S
            LABEL RECORD STANDARD
            BLOCK 0
            RECORDING F.
        01  FD-REG-RUEC009S             PIC  X(530).

      *----------------------------------------------------------------*
      *    OUTPUT: ARQUIVO DE CONTRATOS SELECIONADAS NO RUECV010       *
      *            ORG. SEQUENCIAL   -   LRECL = 038                   *
      *----------------------------------------------------------------*

       FD  RUEC010S
           LABEL RECORD STANDARD
           BLOCK 0
           RECORDING F.

       01  FD-REG-RUEC010S             PIC  X(038).

      *----------------------------------------------------------------*
      *    OUTPUT: ARQUIVO DE CONTRATOS SELECIONADAS NO RUECV011       *
      *            ORG. SEQUENCIAL   -   LRECL = 066                   *
      *----------------------------------------------------------------*

       FD  RUEC011S
           LABEL RECORD STANDARD
           BLOCK 0
           RECORDING F.

       01  FD-REG-RUEC011S             PIC  X(066).

      *----------------------------------------------------------------*
      *    OUTPUT: ARQUIVO DE CONTRATOS SELECIONADAS NO RUECV012       *
      *            ORG. SEQUENCIAL   -   LRECL = 034                   *
      *----------------------------------------------------------------*

       FD  RUEC012S
           LABEL RECORD STANDARD
           BLOCK 0
           RECORDING F.

       01  FD-REG-RUEC012S             PIC  X(034).

      *----------------------------------------------------------------*
      *    OUTPUT: ARQUIVO DE CONTRATOS SELECIONADAS NO RUECB014       *
      *            ORG. SEQUENCIAL   -   LRECL = 352                   *
      *----------------------------------------------------------------*

       FD  RUEC014S
           LABEL RECORD STANDARD
           BLOCK 0
           RECORDING F.

       01  FD-REG-RUEC014S             PIC  X(352).

      *----------------------------------------------------------------*
      *    OUTPUT: ARQUIVO DE CONTRATOS SELECIONADAS NO RUECB017       *
      *            ORG. SEQUENCIAL   -   LRECL = 610                   *
      *----------------------------------------------------------------*

       FD  RUEC017S
           LABEL RECORD STANDARD
           BLOCK 0
           RECORDING F.

       01  FD-REG-RUEC017S             PIC  X(610).

      *----------------------------------------------------------------*
      *    OUTPUT: ARQUIVO DE CONTRATOS SELECIONADAS NO RUECV018       *
      *            ORG. SEQUENCIAL   -   LRECL = 051                   *
      *----------------------------------------------------------------*

       FD  RUEC018S
           LABEL RECORD STANDARD
           BLOCK 0
           RECORDING F.

       01  FD-REG-RUEC018S             PIC  X(051).

      *----------------------------------------------------------------*
      *    OUTPUT: ARQUIVO DE CONTRATOS SELECIONADAS NO RUECV019       *
      *            ORG. SEQUENCIAL   -   LRECL = 054                   *
      *----------------------------------------------------------------*

       FD  RUEC019S
           LABEL RECORD STANDARD
           BLOCK 0
           RECORDING F.

       01  FD-REG-RUEC019S             PIC  X(054).

      *----------------------------------------------------------------*
      *    OUTPUT: ARQUIVO DE CONTRATOS SELECIONADAS NO RUECV022       *
      *            ORG. SEQUENCIAL   -   LRECL = 076                   *
      *----------------------------------------------------------------*

       FD  RUEC022S
           LABEL RECORD STANDARD
           BLOCK 0
           RECORDING F.

       01  FD-REG-RUEC022S             PIC  X(087).

      *----------------------------------------------------------------*
      *    OUTPUT: ARQUIVO DE CONTRATOS SELECIONADAS NO RUECV023       *
      *            ORG. SEQUENCIAL   -   LRECL = 057                   *
      *----------------------------------------------------------------*

       FD  RUEC023S
           LABEL RECORD STANDARD
           BLOCK 0
           RECORDING F.

       01  FD-REG-RUEC023S             PIC  X(057).

      *----------------------------------------------------------------*
      *    OUTPUT: ARQUIVO DE CONTRATOS SELECIONADAS NO RUECB024       *
      *            ORG. SEQUENCIAL   -   LRECL = 086                   *
      *----------------------------------------------------------------*

       FD  RUEC024S
           LABEL RECORD STANDARD
           BLOCK 0
           RECORDING F.

       01  FD-REG-RUEC024S             PIC  X(086).

      *----------------------------------------------------------------*
      *    OUTPUT: ARQUIVO DE CONTRATOS SELECIONADAS NO RUECV025       *
      *            ORG. SEQUENCIAL   -   LRECL = 080                   *
      *----------------------------------------------------------------*

       FD  RUEC025S
           LABEL RECORD STANDARD
           BLOCK 0
           RECORDING F.

       01  FD-REG-RUEC025S             PIC  X(080).

      *----------------------------------------------------------------*
      *    OUTPUT: ARQUIVO DE CONTRATOS SELECIONADAS NO RUECV026       *
      *            ORG. SEQUENCIAL   -   LRECL = 060                   *
      *----------------------------------------------------------------*

       FD  RUEC026S
           LABEL RECORD STANDARD
           BLOCK 0
           RECORDING F.

       01  FD-REG-RUEC026S             PIC  X(060).

      *----------------------------------------------------------------*
      *    OUTPUT: ARQUIVO DE CONTRATOS SELECIONADAS NO RUECB027       *
      *            ORG. SEQUENCIAL   -   LRECL = 089                   *
      *----------------------------------------------------------------*

       FD  RUEC027S
           LABEL RECORD STANDARD
           BLOCK 0
           RECORDING F.

       01  FD-REG-RUEC027S             PIC  X(089).

      *----------------------------------------------------------------*
      *    OUTPUT: ARQUIVO DE CONTRATOS SELECIONADAS NO RUECB028       *
      *            ORG. SEQUENCIAL   -   LRECL = 191                   *
      *----------------------------------------------------------------*

       FD  RUEC028S
           LABEL RECORD STANDARD
           BLOCK 0
           RECORDING F.

       01  FD-REG-RUEC028S             PIC  X(191).

      *----------------------------------------------------------------*
      *    OUTPUT: ARQUIVO DE CONTRATOS SELECIONADAS NO RUECB033       *
      *            ORG. SEQUENCIAL   -   LRECL = 019                   *
      *----------------------------------------------------------------*

       FD  RUEC033S
           LABEL RECORD STANDARD
           BLOCK 0
           RECORDING F.

       01  FD-REG-RUEC033S             PIC  X(019).

      *---------------------------------------------------------------*
      *    OUTPUT: ARQUIVO DE CONTRATOS SELECIONADAS NO RUECB034      *
      *            ORG. SEQUENCIAL   -   LRECL = 250                  *
      *---------------------------------------------------------------*

       FD  RUEC034S
           LABEL RECORD STANDARD
           BLOCK 0
           RECORDING F.

       01  FD-REG-RUEC034S             PIC  X(250).

      *----------------------------------------------------------------*
      *    OUTPUT: ARQUIVO DE CONTRATOS SELECIONADAS NO RUECB035       *
      *            ORG. SEQUENCIAL   -   LRECL = 612                   *
      *----------------------------------------------------------------*

       FD  RUEC035S
           LABEL RECORD STANDARD
           BLOCK 0
           RECORDING F.

       01  FD-REG-RUEC035S             PIC  X(612).

      *----------------------------------------------------------------*
      *    OUTPUT: ARQUIVO DE CONTRATOS SELECIONADAS NO RUECB036       *
      *            ORG. SEQUENCIAL   -   LRECL = 092                   *
      *----------------------------------------------------------------*

       FD  RUEC036S
           LABEL RECORD STANDARD
           BLOCK 0
           RECORDING F.

       01  FD-REG-RUEC036S             PIC  X(092).

      *----------------------------------------------------------------*
      *    OUTPUT: ARQUIVO DE CONTRATOS SELECIONADAS NO RUECB038       *
      *            ORG. SEQUENCIAL   -   LRECL = 089                   *
      *----------------------------------------------------------------*

       FD  RUEC038S
           LABEL RECORD STANDARD
           BLOCK 0
           RECORDING F.

       01  FD-REG-RUEC038S             PIC  X(089).

      *----------------------------------------------------------------*
      *    OUTPUT: ARQUIVO DE CONTRATOS SELECIONADAS NO RUECB040       *
      *            ORG. SEQUENCIAL   -   LRECL = 092                   *
      *----------------------------------------------------------------*

       FD  RUEC040S
           LABEL RECORD STANDARD
           BLOCK 0
           RECORDING F.

       01  FD-REG-RUEC040S             PIC  X(092).

      *----------------------------------------------------------------*
      *    OUTPUT: ARQUIVO DE CONTRATOS SELECIONADAS NO RUECB041       *
      *            ORG. SEQUENCIAL   -   LRECL = 095                   *
      *----------------------------------------------------------------*

       FD  RUEC041S
           LABEL RECORD STANDARD
           BLOCK 0
           RECORDING F.

       01  FD-REG-RUEC041S             PIC  X(095).

      *----------------------------------------------------------------*
      *    OUTPUT: ARQUIVO DE CONTRATOS SELECIONADAS NO RUECB044       *
      *            ORG. SEQUENCIAL   -   LRECL = 490                   *
      *----------------------------------------------------------------*

       FD  RUEC044S
           LABEL RECORD STANDARD
           BLOCK 0
           RECORDING F.

       01  FD-REG-RUEC044S             PIC  X(490).

      *----------------------------------------------------------------*
      *    OUTPUT: ARQUIVO DE CONTRATOS SELECIONADAS NO RUECB052       *
      *            ORG. SEQUENCIAL   -   LRECL = 842                   *
      *----------------------------------------------------------------*

       FD  RUEC052S
           LABEL RECORD STANDARD
           BLOCK 0
           RECORDING F.

       01  FD-REG-RUEC052S             PIC  X(842).

      *----------------------------------------------------------------*
      *    OUTPUT: ARQUIVO DE CONTRATOS SELECIONADAS NO RUECB053       *
      *            ORG. SEQUENCIAL   -   LRECL = 085                   *
      *----------------------------------------------------------------*

       FD  RUEC053S
           LABEL RECORD STANDARD
           BLOCK 0
           RECORDING F.

       01  FD-REG-RUEC053S             PIC  X(085).

      *----------------------------------------------------------------*
      *    OUTPUT: ARQUIVO DE CONTRATOS SELECIONADAS NO RUECB054       *
      *            ORG. SEQUENCIAL   -   LRECL = 057                   *
      *----------------------------------------------------------------*

       FD  RUEC054S
           LABEL RECORD STANDARD
           BLOCK 0
           RECORDING F.

       01  FD-REG-RUEC054S             PIC  X(057).

      *----------------------------------------------------------------*
      *    OUTPUT: ARQUIVO DE CONTRATOS SELECIONADAS NO RUECB064       *
      *            ORG. SEQUENCIAL   -   LRECL = 1218                  *
      *----------------------------------------------------------------*

       FD  RUEC064S
           LABEL RECORD STANDARD
           BLOCK 0
           RECORDING F.

       01  FD-REG-RUEC064S             PIC  X(1218).

      *----------------------------------------------------------------*
      *    OUTPUT: ARQUIVO DE CONTRATOS SELECIONADAS NO RUECB065       *
      *            ORG. SEQUENCIAL   -   LRECL = 471                   *
      *----------------------------------------------------------------*

       FD  RUEC065S
           LABEL RECORD STANDARD
           BLOCK 0
           RECORDING F.

       01  FD-REG-RUEC065S             PIC  X(471).

      *----------------------------------------------------------------*
      *    OUTPUT: ARQUIVO DE CONTRATOS SELECIONADAS NO RUECB066       *
      *            ORG. SEQUENCIAL   -   LRECL = 134                   *
      *----------------------------------------------------------------*

       FD  RUEC066S
           LABEL RECORD STANDARD
           BLOCK 0
           RECORDING F.

       01  FD-REG-RUEC066S             PIC  X(134).

      *----------------------------------------------------------------*
      *    OUTPUT: ARQUIVO DE CONTRATOS SELECIONADAS NO RUECB073       *
      *            ORG. SEQUENCIAL   -   LRECL = 082                   *
      *----------------------------------------------------------------*

       FD  RUEC073S
           LABEL RECORD STANDARD
           BLOCK 0
           RECORDING F.

       01  FD-REG-RUEC073S             PIC  X(082).

      *----------------------------------------------------------------*
      *    OUTPUT: ARQUIVO DE CONTRATOS SELECIONADAS NO RUECB074       *
      *            ORG. SEQUENCIAL   -   LRECL = 153                   *
      *----------------------------------------------------------------*

       FD  RUEC074S
           LABEL RECORD STANDARD
           BLOCK 0
           RECORDING F.

       01  FD-REG-RUEC074S             PIC  X(153).

      *----------------------------------------------------------------*
      *    OUTPUT: ARQUIVO DE CONTRATOS SELECIONADAS NO RUECB075       *
      *            ORG. SEQUENCIAL   -   LRECL = 150                   *
      *----------------------------------------------------------------*

       FD  RUEC075S
           LABEL RECORD STANDARD
           BLOCK 0
           RECORDING F.

       01  FD-REG-RUEC075S             PIC  X(150).

      *----------------------------------------------------------------*
      *    OUTPUT: ARQUIVO DE CONTRATOS SELECIONADAS NO RUECB096       *
      *            ORG. SEQUENCIAL   -   LRECL = 044                   *
      *----------------------------------------------------------------*

       FD  RUEC096S
           LABEL RECORD STANDARD
           BLOCK 0
           RECORDING F.

       01  FD-REG-RUEC096S             PIC  X(044).

AC0613*----------------------------------------------------------------*
AC0613*    OUTPUT: ARQUIVO DE CONTRATOS SELECIONADAS NO RUECB097       *
AC0613*            ORG. SEQUENCIAL   -   LRECL = 161                   *
AC0613*----------------------------------------------------------------*
AC0613
AC0613 FD  RUEC097S
AC0613     LABEL RECORD STANDARD
AC0613     BLOCK 0
AC0613     RECORDING F.
AC0613
AC0613 01  FD-REG-RUEC097S             PIC  X(161).

AC0613*----------------------------------------------------------------*
AC0613*    OUTPUT: ARQUIVO DE CONTRATOS SELECIONADAS NO RUECB099       *
AC0613*            ORG. SEQUENCIAL   -   LRECL = 225                   *
AC0613*----------------------------------------------------------------*
AC0613
AC0613 FD  RUEC099S
AC0613     LABEL RECORD STANDARD
AC0613     BLOCK 0
AC0613     RECORDING F.
AC0613
AC0613 01  FD-REG-RUEC099S             PIC  X(225).

AC0613*----------------------------------------------------------------*
AC0613*    OUTPUT: ARQUIVO DE CONTRATOS SELECIONADAS NO RUECB09A       *
AC0613*            ORG. SEQUENCIAL   -   LRECL = 32.542                *
AC0613*----------------------------------------------------------------*
AC0613
AC0613 FD  RUEC09AS
AC0613     LABEL RECORD STANDARD
AC0613     BLOCK 0
AC0613     RECORDING F.
AC0613
AC0613 01  FD-REG-RUEC09AS             PIC  X(32542).

AC0613*----------------------------------------------------------------*
AC0613*    OUTPUT: ARQUIVO DE CONTRATOS SELECIONADAS NO RUECB09B       *
AC0613*            ORG. SEQUENCIAL   -   LRECL = 140                   *
AC0613*----------------------------------------------------------------*
AC0613
AC0613 FD  RUEC09BS
AC0613     LABEL RECORD STANDARD
AC0613     BLOCK 0
AC0613     RECORDING F.
AC0613
AC0613 01  FD-REG-RUEC09BS             PIC  X(140).

AC0613*----------------------------------------------------------------*
AC0613*    OUTPUT: ARQUIVO DE CONTRATOS SELECIONADAS NO RUECB09D       *
AC0613*            ORG. SEQUENCIAL   -   LRECL = 029                   *
AC0613*----------------------------------------------------------------*
AC0613
AC0613 FD  RUEC09DS
AC0613     LABEL RECORD STANDARD
AC0613     BLOCK 0
AC0613     RECORDING F.
AC0613
AC0613 01  FD-REG-RUEC09DS             PIC  X(029).

AC0613*---------------------------------------------------------------*
AC0613*   OUTPUT: ARQUIVO DE CONTRATOS SELECIONADAS NO RUECB09E       *
AC0613*           ORG. SEQUENCIAL   -   LRECL = 027                   *
AC0613*---------------------------------------------------------------*
AC0613
AC0613 FD  RUEC09ES
AC0613     LABEL RECORD STANDARD
AC0613     BLOCK 0
AC0613     RECORDING F.
AC0613
AC0613 01  FD-REG-RUEC09ES             PIC  X(027).

AC0613*---------------------------------------------------------------*
AC0613*   OUTPUT: ARQUIVO DE CONTRATOS SELECIONADAS NO RUECB09F       *
AC0613*           ORG. SEQUENCIAL   -   LRECL = 014                   *
AC0613*---------------------------------------------------------------*
AC0613
AC0613 FD  RUEC09FS
AC0613     LABEL RECORD STANDARD
AC0613     BLOCK 0
AC0613     RECORDING F.
AC0613
AC0613 01  FD-REG-RUEC09FS             PIC  X(014).

AC0613*---------------------------------------------------------------*
AC0613*   OUTPUT: ARQUIVO DE CONTRATOS SELECIONADAS NO RUECB09G       *
AC0613*           ORG. SEQUENCIAL   -   LRECL = 032                   *
AC0613*---------------------------------------------------------------*
AC0613
AC0613 FD  RUEC09GS
AC0613     LABEL RECORD STANDARD
AC0613     BLOCK 0
AC0613     RECORDING F.
AC0613
AC0613 01  FD-REG-RUEC09GS             PIC  X(032).

AC0613*---------------------------------------------------------------*
AC0613*   OUTPUT: ARQUIVO DE CONTRATOS SELECIONADAS NO RUECB09H       *
AC0613*           ORG. SEQUENCIAL   -   LRECL = 035                   *
AC0613*---------------------------------------------------------------*
AC0613
AC0613 FD  RUEC09HS
AC0613     LABEL RECORD STANDARD
AC0613     BLOCK 0
AC0613     RECORDING F.
AC0613
AC0613 01  FD-REG-RUEC09HS             PIC  X(035).

AC0613*----------------------------------------------------------------*
AC0613*    OUTPUT: ARQUIVO DE CONTRATOS SELECIONADAS NO RUECB09I       *
AC0613*            ORG. SEQUENCIAL   -   LRECL = 220                   *
AC0613*----------------------------------------------------------------*
AC0613
AC0613 FD  RUEC09IS
AC0613     LABEL RECORD STANDARD
AC0613     BLOCK 0
AC0613     RECORDING F.
AC0613
AC0613 01  FD-REG-RUEC09IS             PIC  X(220).

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(032)  VALUE
           '*** INICIO DA WORKING RUEC1450**'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(032)  VALUE
           '* ACUMULADORES                 *'.
      *----------------------------------------------------------------*

       77  ACU-LIDOS-ARQDATAS          PIC  9(09) COMP-3   VALUE ZEROS.
       77  ACU-LIDOS-SELCDULA          PIC  9(09) COMP-3   VALUE ZEROS.
       77  ACU-LIDOS-AGENCIA           PIC  9(09) COMP-3   VALUE ZEROS.
       77  ACU-GRAG-RUEC008S           PIC  9(09) COMP-3   VALUE ZEROS.
       77  ACU-GRAVS-RUEC008S          PIC  9(09) COMP-3   VALUE ZEROS.
       77  ACU-LIDOS-RUEC008S          PIC  9(09) COMP-3   VALUE ZEROS.
       77  ACU-GRAG-RUEC009S           PIC  9(09) COMP-3   VALUE ZEROS.
       77  ACU-GRAVS-RUEC009S          PIC  9(09) COMP-3   VALUE ZEROS.
       77  ACU-LIDOS-RUEC009S          PIC  9(09) COMP-3   VALUE ZEROS.
       77  ACU-GRAG-RUEC010S           PIC  9(09) COMP-3   VALUE ZEROS.
       77  ACU-GRAVS-RUEC010S          PIC  9(09) COMP-3   VALUE ZEROS.
       77  ACU-LIDOS-RUEC010S          PIC  9(09) COMP-3   VALUE ZEROS.
       77  ACU-GRAG-RUEC011S           PIC  9(09) COMP-3   VALUE ZEROS.
       77  ACU-GRAVS-RUEC011S          PIC  9(09) COMP-3   VALUE ZEROS.
       77  ACU-LIDOS-RUEC011S          PIC  9(09) COMP-3   VALUE ZEROS.
       77  ACU-GRAG-RUEC012S           PIC  9(09) COMP-3   VALUE ZEROS.
       77  ACU-GRAVS-RUEC012S          PIC  9(09) COMP-3   VALUE ZEROS.
       77  ACU-LIDOS-RUEC012S          PIC  9(09) COMP-3   VALUE ZEROS.
       77  ACU-GRAG-RUEC014S           PIC  9(09) COMP-3   VALUE ZEROS.
       77  ACU-GRAVS-RUEC014S          PIC  9(09) COMP-3   VALUE ZEROS.
       77  ACU-LIDOS-RUEC014S          PIC  9(09) COMP-3   VALUE ZEROS.
       77  ACU-GRAG-RUEC017S           PIC  9(09) COMP-3   VALUE ZEROS.
       77  ACU-GRAVS-RUEC017S          PIC  9(09) COMP-3   VALUE ZEROS.
       77  ACU-LIDOS-RUEC017S          PIC  9(09) COMP-3   VALUE ZEROS.
       77  ACU-GRAG-RUEC018S           PIC  9(09) COMP-3   VALUE ZEROS.
       77  ACU-GRAVS-RUEC018S          PIC  9(09) COMP-3   VALUE ZEROS.
       77  ACU-LIDOS-RUEC018S          PIC  9(09) COMP-3   VALUE ZEROS.
       77  ACU-GRAG-RUEC019S           PIC  9(09) COMP-3   VALUE ZEROS.
       77  ACU-GRAVS-RUEC019S          PIC  9(09) COMP-3   VALUE ZEROS.
       77  ACU-LIDOS-RUEC019S          PIC  9(09) COMP-3   VALUE ZEROS.
       77  ACU-GRAG-RUEC022S           PIC  9(09) COMP-3   VALUE ZEROS.
       77  ACU-GRAVS-RUEC022S          PIC  9(09) COMP-3   VALUE ZEROS.
       77  ACU-LIDOS-RUEC022S          PIC  9(09) COMP-3   VALUE ZEROS.
       77  ACU-GRAG-RUEC023S           PIC  9(09) COMP-3   VALUE ZEROS.
       77  ACU-GRAVS-RUEC023S          PIC  9(09) COMP-3   VALUE ZEROS.
       77  ACU-LIDOS-RUEC023S          PIC  9(09) COMP-3   VALUE ZEROS.
       77  ACU-GRAG-RUEC024S           PIC  9(09) COMP-3   VALUE ZEROS.
       77  ACU-GRAVS-RUEC024S          PIC  9(09) COMP-3   VALUE ZEROS.
       77  ACU-LIDOS-RUEC024S          PIC  9(09) COMP-3   VALUE ZEROS.
       77  ACU-GRAG-RUEC025S           PIC  9(09) COMP-3   VALUE ZEROS.
       77  ACU-GRAVS-RUEC025S          PIC  9(09) COMP-3   VALUE ZEROS.
       77  ACU-LIDOS-RUEC025S          PIC  9(09) COMP-3   VALUE ZEROS.
       77  ACU-GRAG-RUEC026S           PIC  9(09) COMP-3   VALUE ZEROS.
       77  ACU-GRAVS-RUEC026S          PIC  9(09) COMP-3   VALUE ZEROS.
       77  ACU-LIDOS-RUEC026S          PIC  9(09) COMP-3   VALUE ZEROS.
       77  ACU-GRAG-RUEC027S           PIC  9(09) COMP-3   VALUE ZEROS.
       77  ACU-GRAVS-RUEC027S          PIC  9(09) COMP-3   VALUE ZEROS.
       77  ACU-LIDOS-RUEC027S          PIC  9(09) COMP-3   VALUE ZEROS.
       77  ACU-GRAG-RUEC028S           PIC  9(09) COMP-3   VALUE ZEROS.
       77  ACU-GRAVS-RUEC028S          PIC  9(09) COMP-3   VALUE ZEROS.
       77  ACU-LIDOS-RUEC028S          PIC  9(09) COMP-3   VALUE ZEROS.
       77  ACU-GRAG-RUEC033S           PIC  9(09) COMP-3   VALUE ZEROS.
       77  ACU-GRAVS-RUEC033S          PIC  9(09) COMP-3   VALUE ZEROS.
       77  ACU-LIDOS-RUEC033S          PIC  9(09) COMP-3   VALUE ZEROS.
       77  ACU-GRAG-RUEC034S           PIC  9(09) COMP-3   VALUE ZEROS.
       77  ACU-GRAVS-RUEC034S          PIC  9(09) COMP-3   VALUE ZEROS.
       77  ACU-LIDOS-RUEC034S          PIC  9(09) COMP-3   VALUE ZEROS.
       77  ACU-GRAG-RUEC035S           PIC  9(09) COMP-3   VALUE ZEROS.
       77  ACU-GRAVS-RUEC035S          PIC  9(09) COMP-3   VALUE ZEROS.
       77  ACU-LIDOS-RUEC035S          PIC  9(09) COMP-3   VALUE ZEROS.
       77  ACU-GRAG-RUEC036S           PIC  9(09) COMP-3   VALUE ZEROS.
       77  ACU-GRAVS-RUEC036S          PIC  9(09) COMP-3   VALUE ZEROS.
       77  ACU-LIDOS-RUEC036S          PIC  9(09) COMP-3   VALUE ZEROS.
       77  ACU-GRAG-RUEC038S           PIC  9(09) COMP-3   VALUE ZEROS.
       77  ACU-GRAVS-RUEC038S          PIC  9(09) COMP-3   VALUE ZEROS.
       77  ACU-LIDOS-RUEC038S          PIC  9(09) COMP-3   VALUE ZEROS.
       77  ACU-GRAG-RUEC040S           PIC  9(09) COMP-3   VALUE ZEROS.
       77  ACU-GRAVS-RUEC040S          PIC  9(09) COMP-3   VALUE ZEROS.
       77  ACU-LIDOS-RUEC040S          PIC  9(09) COMP-3   VALUE ZEROS.
       77  ACU-GRAG-RUEC041S           PIC  9(09) COMP-3   VALUE ZEROS.
       77  ACU-GRAVS-RUEC041S          PIC  9(09) COMP-3   VALUE ZEROS.
       77  ACU-LIDOS-RUEC041S          PIC  9(09) COMP-3   VALUE ZEROS.
       77  ACU-GRAG-RUEC044S           PIC  9(09) COMP-3   VALUE ZEROS.
       77  ACU-GRAVS-RUEC044S          PIC  9(09) COMP-3   VALUE ZEROS.
       77  ACU-LIDOS-RUEC044S          PIC  9(09) COMP-3   VALUE ZEROS.
       77  ACU-GRAG-RUEC052S           PIC  9(09) COMP-3   VALUE ZEROS.
       77  ACU-GRAVS-RUEC052S          PIC  9(09) COMP-3   VALUE ZEROS.
       77  ACU-LIDOS-RUEC052S          PIC  9(09) COMP-3   VALUE ZEROS.
       77  ACU-GRAG-RUEC053S           PIC  9(09) COMP-3   VALUE ZEROS.
       77  ACU-GRAVS-RUEC053S          PIC  9(09) COMP-3   VALUE ZEROS.
       77  ACU-LIDOS-RUEC053S          PIC  9(09) COMP-3   VALUE ZEROS.
       77  ACU-GRAG-RUEC054S           PIC  9(09) COMP-3   VALUE ZEROS.
       77  ACU-GRAVS-RUEC054S          PIC  9(09) COMP-3   VALUE ZEROS.
       77  ACU-LIDOS-RUEC054S          PIC  9(09) COMP-3   VALUE ZEROS.
       77  ACU-GRAG-RUEC064S           PIC  9(09) COMP-3   VALUE ZEROS.
       77  ACU-GRAVS-RUEC064S          PIC  9(09) COMP-3   VALUE ZEROS.
       77  ACU-LIDOS-RUEC064S          PIC  9(09) COMP-3   VALUE ZEROS.
       77  ACU-GRAG-RUEC065S           PIC  9(09) COMP-3   VALUE ZEROS.
       77  ACU-GRAVS-RUEC065S          PIC  9(09) COMP-3   VALUE ZEROS.
       77  ACU-LIDOS-RUEC065S          PIC  9(09) COMP-3   VALUE ZEROS.
       77  ACU-GRAG-RUEC066S           PIC  9(09) COMP-3   VALUE ZEROS.
       77  ACU-GRAVS-RUEC066S          PIC  9(09) COMP-3   VALUE ZEROS.
       77  ACU-LIDOS-RUEC066S          PIC  9(09) COMP-3   VALUE ZEROS.
       77  ACU-GRAG-RUEC073S           PIC  9(09) COMP-3   VALUE ZEROS.
       77  ACU-GRAVS-RUEC073S          PIC  9(09) COMP-3   VALUE ZEROS.
       77  ACU-LIDOS-RUEC073S          PIC  9(09) COMP-3   VALUE ZEROS.
       77  ACU-GRAG-RUEC074S           PIC  9(09) COMP-3   VALUE ZEROS.
       77  ACU-GRAVS-RUEC074S          PIC  9(09) COMP-3   VALUE ZEROS.
       77  ACU-LIDOS-RUEC074S          PIC  9(09) COMP-3   VALUE ZEROS.
       77  ACU-GRAG-RUEC075S           PIC  9(09) COMP-3   VALUE ZEROS.
       77  ACU-GRAVS-RUEC075S          PIC  9(09) COMP-3   VALUE ZEROS.
       77  ACU-LIDOS-RUEC075S          PIC  9(09) COMP-3   VALUE ZEROS.
       77  ACU-GRAG-RUEC096S           PIC  9(09) COMP-3   VALUE ZEROS.
       77  ACU-GRAVS-RUEC096S          PIC  9(09) COMP-3   VALUE ZEROS.
       77  ACU-LIDOS-RUEC096S          PIC  9(09) COMP-3   VALUE ZEROS.
AC0613 77  ACU-GRAG-RUEC097S           PIC  9(09) COMP-3   VALUE ZEROS.
AC0613 77  ACU-GRAVS-RUEC097S          PIC  9(09) COMP-3   VALUE ZEROS.
AC0613 77  ACU-LIDOS-RUEC097S          PIC  9(09) COMP-3   VALUE ZEROS.
AC0613 77  ACU-GRAG-RUEC099S           PIC  9(09) COMP-3   VALUE ZEROS.
AC0613 77  ACU-GRAVS-RUEC099S          PIC  9(09) COMP-3   VALUE ZEROS.
AC0613 77  ACU-LIDOS-RUEC099S          PIC  9(09) COMP-3   VALUE ZEROS.
AC0613 77  ACU-GRAG-RUEC09AS           PIC  9(09) COMP-3   VALUE ZEROS.
AC0613 77  ACU-GRAVS-RUEC09AS          PIC  9(09) COMP-3   VALUE ZEROS.
AC0613 77  ACU-LIDOS-RUEC09AS          PIC  9(09) COMP-3   VALUE ZEROS.
AC0613 77  ACU-GRAG-RUEC09BS           PIC  9(09) COMP-3   VALUE ZEROS.
AC0613 77  ACU-GRAVS-RUEC09BS          PIC  9(09) COMP-3   VALUE ZEROS.
AC0613 77  ACU-LIDOS-RUEC09BS          PIC  9(09) COMP-3   VALUE ZEROS.
AC0613 77  ACU-GRAG-RUEC09DS           PIC  9(09) COMP-3   VALUE ZEROS.
AC0613 77  ACU-GRAVS-RUEC09DS          PIC  9(09) COMP-3   VALUE ZEROS.
AC0613 77  ACU-LIDOS-RUEC09DS          PIC  9(09) COMP-3   VALUE ZEROS.
AC0613 77  ACU-GRAG-RUEC09ES           PIC  9(09) COMP-3   VALUE ZEROS.
AC0613 77  ACU-GRAVS-RUEC09ES          PIC  9(09) COMP-3   VALUE ZEROS.
AC0613 77  ACU-LIDOS-RUEC09ES          PIC  9(09) COMP-3   VALUE ZEROS.
AC0613 77  ACU-GRAG-RUEC09FS           PIC  9(09) COMP-3   VALUE ZEROS.
AC0613 77  ACU-GRAVS-RUEC09FS          PIC  9(09) COMP-3   VALUE ZEROS.
AC0613 77  ACU-LIDOS-RUEC09FS          PIC  9(09) COMP-3   VALUE ZEROS.
AC0613 77  ACU-GRAG-RUEC09GS           PIC  9(09) COMP-3   VALUE ZEROS.
AC0613 77  ACU-GRAVS-RUEC09GS          PIC  9(09) COMP-3   VALUE ZEROS.
AC0613 77  ACU-LIDOS-RUEC09GS          PIC  9(09) COMP-3   VALUE ZEROS.
AC0613 77  ACU-GRAG-RUEC09HS           PIC  9(09) COMP-3   VALUE ZEROS.
AC0613 77  ACU-GRAVS-RUEC09HS          PIC  9(09) COMP-3   VALUE ZEROS.
AC0613 77  ACU-LIDOS-RUEC09HS          PIC  9(09) COMP-3   VALUE ZEROS.
AC0613 77  ACU-GRAG-RUEC09IS           PIC  9(09) COMP-3   VALUE ZEROS.
AC0613 77  ACU-GRAVS-RUEC09IS          PIC  9(09) COMP-3   VALUE ZEROS.
AC0613 77  ACU-LIDOS-RUEC09IS          PIC  9(09) COMP-3   VALUE ZEROS.


      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(032)  VALUE
           '* AREAS AUX. TRATAMENTO ERROS  *'.
      *----------------------------------------------------------------*

       77  WRK-BATCH                   PIC  X(08) VALUE 'BATCH'.

       77  WRK-FS-RUEC008S             PIC  X(02)         VALUE SPACES.
       77  WRK-FS-RUEC009S             PIC  X(02)         VALUE SPACES.
       77  WRK-FS-RUEC010S             PIC  X(02)         VALUE SPACES.
       77  WRK-FS-RUEC011S             PIC  X(02)         VALUE SPACES.
       77  WRK-FS-RUEC012S             PIC  X(02)         VALUE SPACES.
       77  WRK-FS-RUEC014S             PIC  X(02)         VALUE SPACES.
       77  WRK-FS-RUEC017S             PIC  X(02)         VALUE SPACES.
       77  WRK-FS-RUEC018S             PIC  X(02)         VALUE SPACES.
       77  WRK-FS-RUEC019S             PIC  X(02)         VALUE SPACES.
       77  WRK-FS-RUEC022S             PIC  X(02)         VALUE SPACES.
       77  WRK-FS-RUEC023S             PIC  X(02)         VALUE SPACES.
       77  WRK-FS-RUEC024S             PIC  X(02)         VALUE SPACES.
       77  WRK-FS-RUEC025S             PIC  X(02)         VALUE SPACES.
       77  WRK-FS-RUEC026S             PIC  X(02)         VALUE SPACES.
       77  WRK-FS-RUEC027S             PIC  X(02)         VALUE SPACES.
       77  WRK-FS-RUEC028S             PIC  X(02)         VALUE SPACES.
       77  WRK-FS-RUEC033S             PIC  X(02)         VALUE SPACES.
       77  WRK-FS-RUEC034S             PIC  X(02)         VALUE SPACES.
       77  WRK-FS-RUEC035S             PIC  X(02)         VALUE SPACES.
       77  WRK-FS-RUEC036S             PIC  X(02)         VALUE SPACES.
       77  WRK-FS-RUEC038S             PIC  X(02)         VALUE SPACES.
       77  WRK-FS-RUEC040S             PIC  X(02)         VALUE SPACES.
       77  WRK-FS-RUEC041S             PIC  X(02)         VALUE SPACES.
       77  WRK-FS-RUEC044S             PIC  X(02)         VALUE SPACES.
       77  WRK-FS-RUEC052S             PIC  X(02)         VALUE SPACES.
       77  WRK-FS-RUEC053S             PIC  X(02)         VALUE SPACES.
       77  WRK-FS-RUEC054S             PIC  X(02)         VALUE SPACES.
       77  WRK-FS-RUEC064S             PIC  X(02)         VALUE SPACES.
       77  WRK-FS-RUEC065S             PIC  X(02)         VALUE SPACES.
       77  WRK-FS-RUEC066S             PIC  X(02)         VALUE SPACES.
       77  WRK-FS-RUEC073S             PIC  X(02)         VALUE SPACES.
       77  WRK-FS-RUEC074S             PIC  X(02)         VALUE SPACES.
       77  WRK-FS-RUEC075S             PIC  X(02)         VALUE SPACES.
       77  WRK-FS-RUEC096S             PIC  X(02)         VALUE SPACES.
AC0613 77  WRK-FS-RUEC097S             PIC  X(02)         VALUE SPACES.
AC0613 77  WRK-FS-RUEC099S             PIC  X(02)         VALUE SPACES.
AC0613 77  WRK-FS-RUEC09AS             PIC  X(02)         VALUE SPACES.
AC0613 77  WRK-FS-RUEC09BS             PIC  X(02)         VALUE SPACES.
AC0613 77  WRK-FS-RUEC09DS             PIC  X(02)         VALUE SPACES.
AC0613 77  WRK-FS-RUEC09ES             PIC  X(02)         VALUE SPACES.
AC0613 77  WRK-FS-RUEC09FS             PIC  X(02)         VALUE SPACES.
AC0613 77  WRK-FS-RUEC09GS             PIC  X(02)         VALUE SPACES.
AC0613 77  WRK-FS-RUEC09HS             PIC  X(02)         VALUE SPACES.
AC0613 77  WRK-FS-RUEC09IS             PIC  X(02)         VALUE SPACES.
       77  WRK-FS-ARQDATAS             PIC  X(02)         VALUE SPACES.
       77  WRK-FS-SELCDULA             PIC  X(02)         VALUE SPACES.

       77  WRK-ABERTURA                PIC  X(13) VALUE ' NA ABERTURA'.
       77  WRK-LEITURA                 PIC  X(13) VALUE ' NA LEITURA'.
       77  WRK-GRAVACAO                PIC  X(13) VALUE ' NA GRAVACAO'.
       77  WRK-FECHAMENTO              PIC  X(13) VALUE 'NO FECHAMENTO'.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(032)  VALUE
           '* AREA DA BRAD0160             *'.
      *----------------------------------------------------------------*

       77  WRK-JOBNOME                 PIC  X(08)          VALUE SPACES.
       77  WRK-VALORFAC                PIC  9(05)V99 COMP-3 VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)          VALUE
           '* AREAS AUXILIARES GERAIS      *'.
      *----------------------------------------------------------------*

       01  FILLER.
           05  WRK-FIM-ARQDATAS       PIC  X(01)           VALUE SPACES.
           05  WRK-FIM-SELCDULA       PIC  X(01)           VALUE SPACES.
           05  WRK-FIM-RUEC008S       PIC  X(01)           VALUE SPACES.
           05  WRK-FIM-RUEC009S       PIC  X(01)           VALUE SPACES.
           05  WRK-FIM-RUEC010S       PIC  X(01)           VALUE SPACES.
           05  WRK-FIM-RUEC011S       PIC  X(01)           VALUE SPACES.
           05  WRK-FIM-RUEC012S       PIC  X(01)           VALUE SPACES.
           05  WRK-FIM-RUEC014S       PIC  X(01)           VALUE SPACES.
           05  WRK-FIM-RUEC017S       PIC  X(01)           VALUE SPACES.
           05  WRK-FIM-RUEC018S       PIC  X(01)           VALUE SPACES.
           05  WRK-FIM-RUEC019S       PIC  X(01)           VALUE SPACES.
           05  WRK-FIM-RUEC022S       PIC  X(01)           VALUE SPACES.
           05  WRK-FIM-RUEC023S       PIC  X(01)           VALUE SPACES.
           05  WRK-FIM-RUEC024S       PIC  X(01)           VALUE SPACES.
           05  WRK-FIM-RUEC025S       PIC  X(01)           VALUE SPACES.
           05  WRK-FIM-RUEC026S       PIC  X(01)           VALUE SPACES.
           05  WRK-FIM-RUEC027S       PIC  X(01)           VALUE SPACES.
           05  WRK-FIM-RUEC028S       PIC  X(01)           VALUE SPACES.
           05  WRK-FIM-RUEC033S       PIC  X(01)           VALUE SPACES.
           05  WRK-FIM-RUEC034S       PIC  X(01)           VALUE SPACES.
           05  WRK-FIM-RUEC035S       PIC  X(01)           VALUE SPACES.
           05  WRK-FIM-RUEC036S       PIC  X(01)           VALUE SPACES.
           05  WRK-FIM-RUEC038S       PIC  X(01)           VALUE SPACES.
           05  WRK-FIM-RUEC040S       PIC  X(01)           VALUE SPACES.
           05  WRK-FIM-RUEC041S       PIC  X(01)           VALUE SPACES.
           05  WRK-FIM-RUEC044S       PIC  X(01)           VALUE SPACES.
           05  WRK-FIM-RUEC052S       PIC  X(01)           VALUE SPACES.
           05  WRK-FIM-RUEC053S       PIC  X(01)           VALUE SPACES.
           05  WRK-FIM-RUEC054S       PIC  X(01)           VALUE SPACES.
           05  WRK-FIM-RUEC064S       PIC  X(01)           VALUE SPACES.
           05  WRK-FIM-RUEC065S       PIC  X(01)           VALUE SPACES.
           05  WRK-FIM-RUEC066S       PIC  X(01)           VALUE SPACES.
           05  WRK-FIM-RUEC073S       PIC  X(01)           VALUE SPACES.
           05  WRK-FIM-RUEC074S       PIC  X(01)           VALUE SPACES.
           05  WRK-FIM-RUEC075S       PIC  X(01)           VALUE SPACES.
           05  WRK-FIM-RUEC096S       PIC  X(01)           VALUE SPACES.
AC0613     05  WRK-FIM-RUEC097S       PIC  X(01)           VALUE SPACES.
AC0613     05  WRK-FIM-RUEC099S       PIC  X(01)           VALUE SPACES.
AC0613     05  WRK-FIM-RUEC09AS       PIC  X(01)           VALUE SPACES.
AC0613     05  WRK-FIM-RUEC09BS       PIC  X(01)           VALUE SPACES.
AC0613     05  WRK-FIM-RUEC09DS       PIC  X(01)           VALUE SPACES.
AC0613     05  WRK-FIM-RUEC09ES       PIC  X(01)           VALUE SPACES.
AC0613     05  WRK-FIM-RUEC09FS       PIC  X(01)           VALUE SPACES.
AC0613     05  WRK-FIM-RUEC09GS       PIC  X(01)           VALUE SPACES.
AC0613     05  WRK-FIM-RUEC09HS       PIC  X(01)           VALUE SPACES.
AC0613     05  WRK-FIM-RUEC09IS       PIC  X(01)           VALUE SPACES.
           05  WRK-CJUNC-DEPDC-ANT    PIC S9(5)V  COMP-3    VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)          VALUE
           '* AREAS AUXILIARES PARA DATAS  *'.
      *----------------------------------------------------------------*
       01  WRK-COMPILACAO.
           05  WRK-DTCOMPILA.
               10  WRK-MMCOMPILA       PIC  X(002)         VALUE SPACES.
               10  FILLER              PIC  X(001)         VALUE SPACES.
               10  WRK-DDCOMPILA       PIC  X(002)         VALUE SPACES.
               10  FILLER              PIC  X(001)         VALUE SPACES.
               10  WRK-AACOMPILA       PIC  X(002)         VALUE SPACES.
           05  WRK-HRCOMPILA           PIC  X(008)         VALUE SPACES.

       01  WRK-DATA-ISO-N              PIC  9(009)         VALUE ZEROS.
       01  WRK-DATA-ISO    REDEFINES   WRK-DATA-ISO-N.
           05  FILLER                  PIC  X(001).
           05  WRK-ANO-ISO             PIC  X(004).
           05  WRK-MES-ISO             PIC  X(002).
           05  WRK-DIA-ISO             PIC  X(002).

       01  WRK-DATA-EDIT.
           05  WRK-DIA-EDIT            PIC  X(002)         VALUE SPACES.
           05  FILLER                  PIC  X(001)         VALUE '/'.
           05  WRK-MES-EDIT            PIC  X(002)         VALUE SPACES.
           05  FILLER                  PIC  X(001)         VALUE '/'.
           05  WRK-ANO-EDIT            PIC  X(004)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)          VALUE
           '* MENSAGEM ARQUIVO VAZIO       *'.
      *----------------------------------------------------------------*

       01  WRK-MSG-ARQ-VAZIO.
           03  FILLER                  PIC  X(08)          VALUE
               'ARQUIVO '.
           03  WRK-ARQUIVO-VAZIO       PIC  X(08)          VALUE SPACES.
           03  FILLER                  PIC  X(59)          VALUE
               ' VAZIO, PROCESSAMENTO ENCERRADO COM ABEND (BRAD7100)'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)          VALUE
           '* MENSAGENS DE ERRO            *'.
      *----------------------------------------------------------------*

       01  WRK-MSG-FS-ERRO.
           03  FILLER                  PIC  X(08)          VALUE SPACES.
           03  FILLER                  PIC  X(02)          VALUE '* '.
           03  FILLER                  PIC  X(05)          VALUE 'ERRO'.
           03  WRK-OPERACAO            PIC  X(13)          VALUE SPACES.
           03  FILLER                  PIC  X(12)          VALUE
               ' DO ARQUIVO '.
           03  WRK-NOME-ARQ            PIC  X(08)          VALUE SPACES.
           03  FILLER                  PIC  X(17)          VALUE
               ' - FILE-STATUS = '.
           03  WRK-FILE-STATUS         PIC  X(02)          VALUE SPACES.
           03  FILLER                  PIC  X(02)          VALUE ' *'.
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32) VALUE
           '* AREA DA BRAD7600             *'.
      *----------------------------------------------------------------*

       01  WRK-DATA-HORA.
           03  WRK-DT-JULIANA          PIC 9(05) COMP-3 VALUE ZEROS.
           03  WRK-DT-AAMMDD           PIC 9(07) COMP-3 VALUE ZEROS.
           03  WRK-DT-AAAAMMDD         PIC 9(09) COMP-3 VALUE ZEROS.
           03  WRK-TI-HHMMSS           PIC 9(07) COMP-3 VALUE ZEROS.
           03  WRK-TI-HHMMSSMMMMMM     PIC 9(13) COMP-3 VALUE ZEROS.
           03  WRK-TIMESTAMP           PIC X(20)       VALUE SPACES.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(32) VALUE
           '* AREA DO ARQUIVO ARQDATAS     *'.
      *----------------------------------------------------------------*

       COPY 'I#RURACR'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32) VALUE
           '* AREA DO ARQUIVO RUECB007     *'.
      *----------------------------------------------------------------*

       01  WRK-RUECB007.
           05  WRK-B007-CJUNC-DEPDC
                                   PIC S9(5)V COMP-3       VALUE ZEROS.
           05  WRK-B007-CCDULA     PIC S9(9)V COMP-3       VALUE ZEROS.
           05  WRK-B007-DCDULA     PIC X(10)              VALUE SPACES.
           05  WRK-B007-CCTA-CORR  PIC S9(7)V COMP-3       VALUE ZEROS.
           05  WRK-B007-CTPO-PROTR-RURAL
                                   PIC S9(2)V COMP-3       VALUE ZEROS.
           05  WRK-B007-CATVDD-RURAL
                                   PIC S9(3)V COMP-3       VALUE ZEROS.
           05  WRK-B007-CFNALD-RURAL
                                   PIC S9(3)V COMP-3       VALUE ZEROS.
           05  WRK-B007-CPRODT-RURAL
                                   PIC S9(3)V COMP-3       VALUE ZEROS.
           05  WRK-B007-QUND-FINCD-RURAL
                                   PIC S9(9)V9(2) COMP-3   VALUE ZEROS.
           05  WRK-B007-CUND-MEDD-FINCD
                                   PIC S9(3)V COMP-3       VALUE ZEROS.
           05  WRK-B007-QUND-PROD-PROVV
                                   PIC S9(9)V9(2) COMP-3   VALUE ZEROS.
           05  WRK-B007-CUND-MEDD-PROVV
                                   PIC S9(3)V COMP-3       VALUE ZEROS.
           05  WRK-B007-DINIC-SAFRA
                                   PIC S9(6)V COMP-3       VALUE ZEROS.
           05  WRK-B007-DFIM-SAFRA PIC S9(6)V COMP-3       VALUE ZEROS.
           05  WRK-B007-VFINAN     PIC S9(13)V9(2) COMP-3  VALUE ZEROS.
           05  WRK-B007-VREC-PPRIO PIC S9(13)V9(2) COMP-3  VALUE ZEROS.
           05  WRK-B007-IPPRIE     PIC X(40)              VALUE SPACES.
           05  WRK-B007-MAREA-TOT  PIC S9(7)V9(2) COMP-3   VALUE ZEROS.
           05  WRK-B007-CMUN-RURAL PIC S9(9)V COMP-3       VALUE ZEROS.
           05  WRK-B007-CESPCE-TITLO-GARNT
                                   PIC S9(3)V COMP-3       VALUE ZEROS.
           05  WRK-B007-CTPO-GARNT PIC S9(3)V COMP-3       VALUE ZEROS.
           05  WRK-B007-CCREA-AGRON-RURAL
                                   PIC S9(10)V COMP-3      VALUE ZEROS.
           05  WRK-B007-CCREA-AGRON-RURAL-NUL
                                   PIC X(1)               VALUE SPACES.
           05  WRK-B007-CRGIAO-CREA
                                   PIC X(02)              VALUE SPACES.
           05  WRK-B007-CRGIAO-CREA-NULL
                                   PIC X(01)              VALUE SPACES.
           05  WRK-B007-CINCID-PROAGRO
                                   PIC S9(2)V COMP-3       VALUE ZEROS.
           05  WRK-B007-CORIGE-REC PIC S9(3)V COMP-3       VALUE ZEROS.
           05  WRK-B007-CGARTD     PIC S9(3)V COMP-3       VALUE ZEROS.
           05  WRK-B007-VCOBER-PROAGRO
                                   PIC S9(13)V9(2) COMP-3  VALUE ZEROS.
           05  WRK-B007-VCOBER-PROAGRO-NULL
                                   PIC X(01)              VALUE SPACES.
           05  WRK-B007-VREC-ENQUA
                                   PIC S9(13)V9(2) COMP-3  VALUE ZEROS.
           05  WRK-B007-VREC-ENQUA-NULL PIC X(01)         VALUE SPACES.
           05  WRK-B007-CESPCE-EXPLO-IMOV
                                        PIC S9(1)V COMP-3  VALUE ZEROS.
           05  WRK-B007-CAUTRZ-OPER    PIC S9(15)V COMP-3  VALUE ZEROS.
           05  WRK-B007-CPROG           PIC X(8)          VALUE SPACES.
           05  WRK-B007-CTPO-CONDC-FSCAL
                                        PIC S9(2)V COMP-3  VALUE ZEROS.
           05  WRK-B007-RESPCE-EXPLO-IMOV  PIC X(15)      VALUE SPACES.
           05  WRK-B007-CAPLIC-RURAL    PIC S9(3)V COMP-3  VALUE ZEROS.
           05  WRK-B007-CCLASF-PRODT-RURAL
                                        PIC S9(5)V COMP-3  VALUE ZEROS.
           05  WRK-B007-CCLASF-PRODT-RURAL-NU PIC X(01)   VALUE SPACES.
           05  WRK-B007-CTPO-EMBAL      PIC S9(3)V COMP-3  VALUE ZEROS.
           05  WRK-B007-CTPO-EMBAL-NULL     PIC X(01)     VALUE SPACES.
           05  WRK-B007-QCAPAC-PROD    PIC S9(11)V COMP-3  VALUE ZEROS.
           05  WRK-B007-QCAPAC-PROD-NULL    PIC X(01)     VALUE SPACES.
           05  WRK-B007-CCATEG-MUTUA    PIC S9(3)V COMP-3  VALUE ZEROS.
           05  WRK-B007-CCATEG-MUTUA-NULL   PIC X(01)     VALUE SPACES.
           05  WRK-B007-VEGF        PIC S9(9)V9(2) COMP-3  VALUE ZEROS.
           05  WRK-B007-VEGF-NULL           PIC X(01)     VALUE SPACES.
           05  WRK-B007-CIMPRE-CDULA-TEMPR
                                        PIC S9(1)V COMP-3  VALUE ZEROS.
           05  WRK-B007-CMATR-IMOV-RURAL     PIC X(15)    VALUE SPACES.
           05  WRK-B007-CPSSOA-CADTR         PIC X(26)    VALUE SPACES.
           05  WRK-B007-CSIT-CDULA-TEMPR
                                       PIC S9(2)V  COMP-3  VALUE ZEROS.
           05  WRK-B007-CINDCD-RESP-APROV
                                        PIC S9(1)V COMP-3  VALUE ZEROS.
           05  WRK-B007-CFUNC-BDSCO     PIC S9(9)V COMP-3  VALUE ZEROS.
           05  WRK-B007-HULT-ATULZ      PIC X(26)         VALUE SPACES.
           05  WRK-B007-CINDCD-TPO-ORCAM
                                        PIC S9(1)V COMP-3  VALUE ZEROS.
           05  WRK-B007-CCONTR          PIC S9(9)V COMP-3  VALUE ZEROS.
           05  WRK-B007-CCONTR-NULL     PIC X(01)         VALUE SPACES.
           05  WRK-B007-DANO-BASE-NEGOC PIC S9(4)V COMP-3  VALUE ZEROS.
           05  WRK-B007-CDEPDC-NEGOC    PIC S9(5)V COMP-3  VALUE ZEROS.
           05  WRK-B007-CSEQ-STUDO-NEGOC
                                        PIC S9(9)V COMP-3  VALUE ZEROS.
           05  WRK-B007-CCLI-UNIC-BDSCO
                                       PIC S9(10)V COMP-3  VALUE ZEROS.
           05  WRK-B007-DULT-VALDC-CDULA        PIC X(26) VALUE SPACES.
           05  WRK-B007-CINDCD-FONE-RESID       PIC X(1)  VALUE SPACES.
           05  WRK-B007-CINDCD-FONE-COML        PIC X(1)  VALUE SPACES.
           05  WRK-B007-CINDCD-ENDER-RESID      PIC X(1)  VALUE SPACES.
           05  WRK-B007-CINDCD-ENDER-COML       PIC X(1)  VALUE SPACES.
           05  WRK-B007-CINDCD-QUALF-PROFS      PIC X(1)  VALUE SPACES.
           05  WRK-B007-QBLOCO-PATRM-IMOV
                                        PIC S9(3)V COMP-3  VALUE ZEROS.
           05  WRK-B007-QBLOCO-PATRM-VEIC
                                        PIC S9(3)V COMP-3  VALUE ZEROS.
           05  WRK-B007-QBLOCO-LSNG-CLI PIC S9(3)V COMP-3  VALUE ZEROS.
           05  WRK-B007-QBLOCO-SEGUR-CLI
                                        PIC S9(3)V COMP-3  VALUE ZEROS.
           05  WRK-B007-QBLOCO-QUADR-SCIAL
                                        PIC S9(3)V COMP-3  VALUE ZEROS.
           05  WRK-B007-DDSPDO-AG               PIC X(10) VALUE SPACES.
           05  WRK-B007-CSEGDR-CREDT-RURAL
                                        PIC S9(3)V COMP-3  VALUE ZEROS.
           05  WRK-B007-DIMPRE-CONTR-RURAL      PIC X(10) VALUE SPACES.
           05  WRK-B007-CSIT-CDULA-ANTER
                                        PIC S9(2)V COMP-3  VALUE ZEROS.
           05  WRK-B007-CINDCD-CTRL-FORML       PIC X(01) VALUE SPACES.
           05  WRK-B007-DANO-LIM-OPER   PIC S9(4)V COMP-3  VALUE ZEROS.
           05  WRK-B007-DANO-LIM-OPER-NULL      PIC X(01) VALUE SPACES.
           05  WRK-B007-CDEPDC-LIM-OPER PIC S9(5)V COMP-3  VALUE ZEROS.
           05  WRK-B007-CDEPDC-LIM-OPER-NULL    PIC X(01) VALUE SPACES.
           05  WRK-B007-CAPROV-LIM-OPER PIC S9(9)V COMP-3  VALUE ZEROS.
           05  WRK-B007-CAPROV-LIM-OPER-NULL    PIC X(01) VALUE SPACES.
           05  WRK-B007-CCESTA-SAFRA-RURAL
                                        PIC S9(5)V  COMP-3  VALUE ZEROS.
           05  WRK-B007-CMODLD-RURAL
                                        PIC S9(5)V  COMP-3  VALUE ZEROS.
           05  WRK-B007-CVAR-PRODT-RURAL
                                        PIC S9(5)V  COMP-3  VALUE ZEROS.
           05  WRK-B007-CINDCD-ZNMTO
                                        PIC S9(1)V  COMP-3  VALUE ZEROS.
           05  WRK-B007-CINDCD-CSCIO
                                        PIC S9(1)V  COMP-3  VALUE ZEROS.
           05  WRK-B007-CTPO-AGROP-RURAL
                                        PIC S9(5)V  COMP-3  VALUE ZEROS.
           05  WRK-B007-CTPO-INTGC-RURAL
                                        PIC S9(5)V  COMP-3  VALUE ZEROS.
           05  WRK-B007-CINDCD-UND-PRDTV
                                        PIC S9(1)V  COMP-3  VALUE ZEROS.
           05  WRK-B007-CTPO-IGCAO-RURAL
                                        PIC S9(5)V  COMP-3  VALUE ZEROS.
           05  WRK-B007-CTPO-CTIVO-RURAL
                                        PIC S9(5)V  COMP-3  VALUE ZEROS.
           05  WRK-B007-CCICLO-PROD-RURAL
                                        PIC S9(5)V  COMP-3  VALUE ZEROS.
           05  WRK-B007-CTPO-CDULA-RECOR    PIC X(3)       VALUE SPACES.
           05  WRK-B007-VRECTA-BRUTA-EMPTO
                                   PIC S9(13)V9(2)  COMP-3  VALUE ZEROS.
           05  WRK-B007-VRECTA-BRUTA-EMPTO-NU   PIC X(01)  VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(42) VALUE
           '* AREA DO ARQUIVO DE NULIDADE RUECB008 *'.
      *----------------------------------------------------------------*

       01  WRK-RUEC008S-NULL.
           05  WRK-B008-VRESP-RURAL-NULL
                                            PIC X(01)     VALUE SPACES.
           05  WRK-B008-VRESP-GRAL-NULL
                                            PIC X(01)     VALUE SPACES.
           05  WRK-B008-VSDO-MED-TMES-NULL
                                            PIC X(01)     VALUE SPACES.
           05  WRK-B008-VSDO-MED-SEMTL-NULL
                                            PIC X(01)     VALUE SPACES.
           05  WRK-B008-VRENDA-AGROP-OUTRO-NU
                                            PIC X(01)     VALUE SPACES.
           05  WRK-B008-VRENDA-NAO-AGROP-NULL
                                            PIC X(01)     VALUE SPACES.
           05  WRK-B008-DCAD-NULL
                                            PIC X(01)     VALUE SPACES.
           05  WRK-B008-DANO-RENDM-NULL
                                            PIC X(01)     VALUE SPACES.
           05  WRK-B008-CSEQ-HIPOT-RURAL-NULL
                                            PIC X(01)     VALUE SPACES.
           05  WRK-B008-CTPO-DOCTO-PSSOA-NULL
                                            PIC X(01)     VALUE SPACES.
           05  WRK-B008-CDOCTO-PSSOA-NULL
                                            PIC X(01)     VALUE SPACES.
           05  WRK-B008-CORG-EMISR-DOCTO-NULL
                                            PIC X(01)     VALUE SPACES.
           05  WRK-B008-CUND-ORG-EMISR-NULL
                                            PIC X(01)     VALUE SPACES.
           05  WRK-B008-CSEXO-NULL
                                            PIC X(01)     VALUE SPACES.
           05  WRK-B008-CEST-CVIL-RGIME-NULL
                                            PIC X(01)     VALUE SPACES.
           05  WRK-B008-DEST-CVIL-NULL
                                            PIC X(01)     VALUE SPACES.
           05  WRK-B008-CNAC-NULL
                                            PIC X(01)     VALUE SPACES.
           05  WRK-B008-CSGL-UF-NULL
                                            PIC X(01)     VALUE SPACES.
           05  WRK-B008-INAT-ESTRG-NULL
                                            PIC X(01)     VALUE SPACES.
           05  WRK-B008-ECOMPL-LOGDR-NULL
                                            PIC X(01)     VALUE SPACES.
           05  WRK-B008-CCATEG-PROFS-CREDT-NU
                                            PIC X(01)     VALUE SPACES.
           05  WRK-B008-CPROFS-PSSOA-NULL
                                            PIC X(01)     VALUE SPACES.
           05  WRK-B008-CCLASS-ATVDD-ECONC-NU
                                            PIC X(01)     VALUE SPACES.
           05  WRK-B008-CRAMO-ATVDD-ECONC-NUL
                                            PIC X(01)     VALUE SPACES.
           05  WRK-B008-CSRAMO-ATVDD-ECONC-NU
                                            PIC X(01)     VALUE SPACES.
           05  WRK-B008-CATVDD-ECONC-NULL
                                            PIC X(01)     VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(42) VALUE
           '* AREA DO ARQUIVO DE NULIDADE RUECB009 *'.
      *----------------------------------------------------------------*

       01  WRK-RUEC009S-NULL.
           05  WRK-B009-RCOMPL-QUALF-GARNT-NU
                                            PIC X(01)     VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(42) VALUE
           '* AREA DO ARQUIVO DE NULIDADE RUECV010 *'.
      *----------------------------------------------------------------*

       01  WRK-RUEC010S-NULL.
           05  WRK-V010-VPCELA-REC-PPRIO-NULL
                                            PIC X(01)     VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(42) VALUE
           '* AREA DO ARQUIVO DE NULIDADE RUECB014 *'.
      *----------------------------------------------------------------*

       01  WRK-RUEC014S-NULL.
           05  WRK-B014-VRESP-RURAL-NULL
                                            PIC X(01)     VALUE SPACES.
           05  WRK-B014-VRESP-GRAL-NULL
                                            PIC X(01)     VALUE SPACES.
           05  WRK-B014-VSDO-MED-TMES-NULL
                                            PIC X(01)     VALUE SPACES.
           05  WRK-B014-VSDO-MED-SEMTL-NULL
                                            PIC X(01)     VALUE SPACES.
           05  WRK-B014-VRENDA-AGROP-OUTRO-NU
                                            PIC X(01)     VALUE SPACES.
           05  WRK-B014-VRENDA-NAO-AGROP-NULL
                                            PIC X(01)     VALUE SPACES.
           05  WRK-B014-DCAD-NULL
                                            PIC X(01)     VALUE SPACES.
           05  WRK-B014-DANO-RENDM-NULL
                                            PIC X(01)     VALUE SPACES.
           05  WRK-B014-CSEQ-HIPOT-RURAL-NULL
                                            PIC X(01)     VALUE SPACES.
           05  WRK-B014-CTPO-DOCTO-PSSOA-NULL
                                            PIC X(01)     VALUE SPACES.
           05  WRK-B014-CDOCTO-PSSOA-NULL
                                            PIC X(01)     VALUE SPACES.
           05  WRK-B014-CORG-EMISR-DOCTO-NULL
                                            PIC X(01)     VALUE SPACES.
           05  WRK-B014-CUND-ORG-EMISR-NULL
                                            PIC X(01)     VALUE SPACES.
           05  WRK-B014-CSEXO-NULL
                                            PIC X(01)     VALUE SPACES.
           05  WRK-B014-CEST-CVIL-RGIME-NULL
                                            PIC X(01)     VALUE SPACES.
           05  WRK-B014-DEST-CVIL-NULL
                                            PIC X(01)     VALUE SPACES.
           05  WRK-B014-CNAC-NULL
                                            PIC X(01)     VALUE SPACES.
           05  WRK-B014-CSGL-UF-NULL
                                            PIC X(01)     VALUE SPACES.
           05  WRK-B014-INAT-ESTRG-NULL
                                            PIC X(01)     VALUE SPACES.
           05  WRK-B014-ECOMPL-LOGDR-NULL
                                            PIC X(01)     VALUE SPACES.
           05  WRK-B014-CCATEG-PROFS-CREDT-NU
                                            PIC X(01)     VALUE SPACES.
           05  WRK-B014-CPROFS-PSSOA-NULL
                                            PIC X(01)     VALUE SPACES.
           05  WRK-B014-CCLASS-ATVDD-ECONC-NU
                                            PIC X(01)     VALUE SPACES.
           05  WRK-B014-CRAMO-ATVDD-ECONC-NUL
                                            PIC X(01)     VALUE SPACES.
           05  WRK-B014-CSRAMO-ATVDD-ECONC-NU
                                            PIC X(01)     VALUE SPACES.
           05  WRK-B014-CATVDD-ECONC-NULL
                                            PIC X(01)     VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(42) VALUE
           '* AREA DO ARQUIVO DE NULIDADE RUECB017 *'.
      *----------------------------------------------------------------*

       01  WRK-RUEC017S-NULL.
           05  WRK-B017-VAVALC-IMOV-NULL
                                            PIC X(01)     VALUE SPACES.
           05  WRK-B017-CSEQ-ESCRT-ONUS-NULL
                                            PIC X(01)     VALUE SPACES.
           05  WRK-B017-CSEQ-PATRM-IMOV-NULL
                                            PIC X(01)     VALUE SPACES.
           05  WRK-B017-CCARTR-IMOV-REG-NULL
                                            PIC X(01)     VALUE SPACES.
           05  WRK-B017-CGLEBA-IMOV-HIPOT-NUL
                                            PIC X(01)     VALUE SPACES.
           05  WRK-B017-CLOTE-IMOV-HIPOT-NULL
                                            PIC X(01)     VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(42) VALUE
           '* AREA DO ARQUIVO DE NULIDADE RUECV022 *'.
      *----------------------------------------------------------------*

       01  WRK-RUEC022S-NULL.
           05  WRK-V022-DLIBRC-OPER-ORCAM-NUL
                                               PIC X(01)   VALUE SPACES.
           05 WRK-V022-DFIM-OPER-ORCAM-NUL     PIC X(01)   VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(42) VALUE
           '* AREA DO ARQUIVO DE NULIDADE RUECV025 *'.
      *----------------------------------------------------------------*

       01  WRK-RUEC025S-NULL.
           05  WRK-V025-DLIBRC-OPER-ORCAM-NUL
                                            PIC X(01)     VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(42) VALUE
           '* AREA DO ARQUIVO DE NULIDADE RUECB035 *'.
      *----------------------------------------------------------------*

       01  WRK-RUEC035S-NULL.
           05  WRK-B035-CSEQ-ESCRT-ONUS-NULL
                                            PIC X(01)     VALUE SPACES.

           05  WRK-B035-CSEQ-PATRM-IMOV-NULL
                                            PIC X(01)     VALUE SPACES.
           05  WRK-B035-CCARTR-IMOV-REG-NULL
                                            PIC X(01)     VALUE SPACES.
           05  WRK-B035-CGLEBA-IMOV-HIPOT-NUL
                                            PIC X(01)     VALUE SPACES.
           05  WRK-B035-CLOTE-IMOV-HIPOT-NULL
                                            PIC X(01)     VALUE SPACES.


      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(42) VALUE
           '* AREA DO ARQUIVO DE NULIDADE RUECB040 *'.
      *----------------------------------------------------------------*

       01  WRK-RUEC040S-NULL.
           05 WRK-B040-CID-NULL          PIC  X(01)      VALUE SPACES.
           05 WRK-B040-MLATTD-NULL       PIC  X(01)      VALUE SPACES.
           05 WRK-B040-MLOGTD-NULL       PIC  X(01)      VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(42) VALUE
           '* AREA DO ARQUIVO DE NULIDADE RUECB041 *'.
      *----------------------------------------------------------------*

       01  WRK-RUEC041S-NULL.
           05 WRK-B041-CID-NULL         PIC  X(01)      VALUE SPACES.
           05 WRK-B041-MLATTD-NULL      PIC  X(01)      VALUE SPACES.
           05 WRK-B041-MLOGTD-NULL      PIC  X(01)      VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(42) VALUE
           '* AREA DO ARQUIVO DE NULIDADE RUECB044 *'.
      *----------------------------------------------------------------*

       01  WRK-RUEC044S-NULL.
           05  WRK-B044-HULT-ATULZ-NULL
                                            PIC X(01)     VALUE SPACES.
           05  WRK-B044-CFUNC-ULT-ATULZ-NULL
                                            PIC X(01)     VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(42) VALUE
           '* AREA DO ARQUIVO DE NULIDADE RUECB052 *'.
      *----------------------------------------------------------------*

       01  WRK-RUEC052S-NULL.
           05  WRK-B052-CMUN-RURAL-NULL
                                            PIC X(01)     VALUE SPACES.
           05  WRK-B052-CCEP-NULL
                                            PIC X(01)     VALUE SPACES.
           05  WRK-B052-CCEP-COMPL-NULL
                                            PIC X(01)     VALUE SPACES.
           05  WRK-B052-RTPO-IMOV-ARREN-NULL
                                            PIC X(01)     VALUE SPACES.
           05  WRK-B052-RTPO-DOCTO-NULL
                                            PIC X(01)     VALUE SPACES.
           05  WRK-B052-CCARTR-IMOV-REG-NULL
                                            PIC X(01)     VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(42) VALUE
           '* AREA DO ARQUIVO DE NULIDADE RUECB064 *'.
      *----------------------------------------------------------------*

       01  WRK-RUEC064S-NULL.
           05  WRK-B064-VFINAN-NULL
                                            PIC X(01)     VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(42) VALUE
           '* AREA DO ARQUIVO DE NULIDADE RUECB065 *'.
      *----------------------------------------------------------------*

       01  WRK-RUEC065S-NULL.
           05  WRK-B065-CREG-PNHOR-RURAL-NULL
                                            PIC X(01)     VALUE SPACES.
           05  WRK-B065-CLIVRO-PNHOR-RURAL-NU
                                            PIC X(01)     VALUE SPACES.
           05  WRK-B065-DREG-PNHOR-RURAL-NULL
                                            PIC X(01)     VALUE SPACES.
           05  WRK-B065-CREG-HIPOT-RURAL-NULL
                                            PIC X(01)     VALUE SPACES.
           05  WRK-B065-CLIVRO-HIPOT-RURAL-NU
                                            PIC X(01)     VALUE SPACES.
           05  WRK-B065-DREG-HIPOT-RURAL-NULL
                                            PIC X(01)     VALUE SPACES.
           05  WRK-B065-DINCL-CARTR-RURAL-NUL
                                            PIC X(01)     VALUE SPACES.
           05  WRK-B065-HULT-ATULZ-NULL
                                            PIC X(01)     VALUE SPACES.
           05  WRK-B065-CFUNC-ULT-ATULZ-NULL
                                            PIC X(01)     VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(42) VALUE
           '* AREA DO ARQUIVO DE NULIDADE RUECB066 *'.
      *----------------------------------------------------------------*

       01  WRK-RUEC066S-NULL.
           05  WRK-B066-DINCL-OBS-LICEN-NULL
                                            PIC X(01)     VALUE SPACES.
           05  WRK-B066-HULT-ATULZ-NULL
                                            PIC X(01)     VALUE SPACES.
           05  WRK-B066-CFUNC-ULT-ATULZ-NULL
                                            PIC X(01)     VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(42) VALUE
           '* AREA DO ARQUIVO DE NULIDADE RUECB074 *'.
      *----------------------------------------------------------------*

       01  WRK-RUEC074S-NULL.
           05  WRK-B074-CORG-EMISR-DOCTO-NULL
                                            PIC X(01)     VALUE SPACES.
           05  WRK-B074-CUND-ORG-EMISR-NULL
                                            PIC X(01)     VALUE SPACES.
           05  WRK-B074-CSGL-UF-NULL
                                            PIC X(01)     VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(42) VALUE
           '* AREA DO ARQUIVO DE NULIDADE RUECB075 *'.
      *----------------------------------------------------------------*

       01  WRK-RUEC075S-NULL.
           05  WRK-B075-CORG-EMISR-DOCTO-NULL
                                            PIC X(01)     VALUE SPACES.
           05  WRK-B075-CUND-ORG-EMISR-NULL
                                            PIC X(01)     VALUE SPACES.
           05  WRK-B075-CSGL-UF-NULL
                                            PIC X(01)     VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(42) VALUE
           '* AREA DO ARQUIVO DE NULIDADE RUECB096 *'.
      *----------------------------------------------------------------*

       01  WRK-RUEC096S-NULL.
           05  WRK-B096-CINDCD-SIT-CDULA-NULL
                                            PIC X(01)     VALUE SPACES.
           05  WRK-B096-CREFT-BACEN-NULL
                                            PIC X(01)     VALUE SPACES.

AC0613*----------------------------------------------------------------*
AC0613 01  FILLER                      PIC  X(42) VALUE
AC0613     '* AREA DO ARQUIVO DE NULIDADE RUECB097 *'.
AC0613*----------------------------------------------------------------*
AC0613
AC0613 01  WRK-RUEC097S-NULL.
AC0613     05  WRK-B097-CUSUAR-NULL   PIC  X(01)       VALUE SPACES.
AC0613     05  WRK-B097-CMODLD-NULL   PIC  X(01)       VALUE SPACES.
AC0613     05  WRK-B097-CPRODT-NULL   PIC  X(01)       VALUE SPACES.
AC0613     05  WRK-B097-CESTCA-NULL   PIC  X(01)       VALUE SPACES.

AC0613*----------------------------------------------------------------*
AC0613 01  FILLER                      PIC  X(42) VALUE
AC0613     '* AREA DO ARQUIVO DE NULIDADE RUECB099 *'.
AC0613*----------------------------------------------------------------*
AC0613
AC0613 01  WRK-RUEC099S-NULL.
AC0613     05  WRK-B099-DCDULA-NULL        PIC  X(01)     VALUE SPACES.
AC0613     05  WRK-B099-DVCTO-NULL         PIC  X(01)     VALUE SPACES.
AC0613     05  WRK-B099-VFINAN-NULL        PIC  X(01)     VALUE SPACES.
AC0613     05  WRK-B099-CCATEG-NULL        PIC  X(01)     VALUE SPACES.
AC0613     05  WRK-B099-CPROG-NULL         PIC  X(01)     VALUE SPACES.
AC0613     05  WRK-B099-CMUN-NULL          PIC  X(01)     VALUE SPACES.
AC0613     05  WRK-B099-LIBRC-NULL         PIC  X(01)     VALUE SPACES.
AC0613     05  WRK-B099-REC-NULL           PIC  X(01)     VALUE SPACES.
AC0613     05  WRK-B099-FINCD-NULL         PIC  X(01)     VALUE SPACES.
AC0613     05  WRK-B099-PROD-NULL          PIC  X(01)     VALUE SPACES.
AC0613     05  WRK-B099-DINIC-NULL         PIC  X(01)     VALUE SPACES.
AC0613     05  WRK-B099-DFIM-NULL          PIC  X(01)     VALUE SPACES.
AC0613     05  WRK-B099-GARNT-NULL         PIC  X(01)     VALUE SPACES.
AC0613
AC0613*----------------------------------------------------------------*
AC0613 01  FILLER                      PIC  X(42) VALUE
AC0613     '* AREA DO ARQUIVO DE NULIDADE RUECB09A *'.
AC0613*----------------------------------------------------------------*
AC0613
AC0613 01  WRK-RUEC09AS-NULL.
AC0613     05  WRK-B09A-CSIT-NULL          PIC  X(01)     VALUE SPACES.
AC0613     05  WRK-B09A-HINCL-NULL         PIC  X(01)     VALUE SPACES.
AC0613
AC0613*----------------------------------------------------------------*
AC0613 01  FILLER                      PIC  X(42) VALUE
AC0613     '* AREA DO ARQUIVO DE NULIDADE RUECB09B *'.
AC0613*----------------------------------------------------------------*
AC0613
AC0613 01  WRK-RUEC09BS-NULL.
AC0613     05  WRK-B09B-CMSGEM-NULL        PIC  X(01)      VALUE SPACES.
AC0613     05  WRK-B09B-HINCL-NULL         PIC  X(01)      VALUE SPACES.
AC0613     05  WRK-B09B-CCTRL-NULL         PIC  X(01)      VALUE SPACES.
AC0613     05  WRK-B09B-CUNIC-NULL         PIC  X(01)      VALUE SPACES.
AC0613     05  WRK-B09B-CREFT-NULL         PIC  X(01)      VALUE SPACES.
AC0613     05  WRK-B09B-CSIT-NULL          PIC  X(01)      VALUE SPACES.
AC0613     05  WRK-B09B-CUSUAR-NULL        PIC  X(01)      VALUE SPACES.
AC0613
AC0613*----------------------------------------------------------------*
AC0613 01  FILLER                      PIC  X(42) VALUE
AC0613     '* AREA DO ARQUIVO DE NULIDADE RUECB09D *'.
AC0613*----------------------------------------------------------------*
AC0613
AC0613 01  WRK-RUEC09DS-NULL.
AC0613     05  WRK-B09D-DVCTO-NULL         PIC  X(01)      VALUE SPACES.
AC0613
AC0613*----------------------------------------------------------------*
AC0613 01  FILLER                      PIC  X(42) VALUE
AC0613     '* AREA DO ARQUIVO DE NULIDADE RUECB09E *'.
AC0613*----------------------------------------------------------------*
AC0613
AC0613 01  WRK-RUEC09ES-NULL.
AC0613     05  WRK-B09E-RESP-NULL          PIC  X(01)      VALUE SPACES.
AC0613     05  WRK-B09E-PSSOA-NULL         PIC  X(01)      VALUE SPACES.
AC0613     05  WRK-B09E-CCNPJ-NULL         PIC  X(01)      VALUE SPACES.
AC0613     05  WRK-B09E-CFLIAL-NULL        PIC  X(01)      VALUE SPACES.
AC0613     05  WRK-B09E-CCTRL-NULL         PIC  X(01)      VALUE SPACES.
AC0613
AC0613*----------------------------------------------------------------*
AC0613 01  FILLER                      PIC  X(42) VALUE
AC0613     '* AREA DO ARQUIVO DE NULIDADE RUECB09F *'.
AC0613*----------------------------------------------------------------*
AC0613
AC0613 01  WRK-RUEC09FS-NULL.
AC0613     05  WRK-B09F-CPRODT-NULL        PIC  X(01)      VALUE SPACES.
AC0613
AC0613*----------------------------------------------------------------*
AC0613 01  FILLER                      PIC  X(42) VALUE
AC0613     '* AREA DO ARQUIVO DE NULIDADE RUECB09G *'.
AC0613*----------------------------------------------------------------*
AC0613
AC0613 01  WRK-RUEC09GS-NULL.
AC0613     05  WRK-B09G-CPRODT-NULL        PIC  X(01)      VALUE SPACES.
AC0613     05  WRK-B09G-DULT-NULL          PIC  X(01)      VALUE SPACES.
AC0613     05  WRK-B09G-CUSUAR-NULL        PIC  X(01)      VALUE SPACES.
AC0613
AC0613*----------------------------------------------------------------*
AC0613 01  FILLER                      PIC  X(42) VALUE
AC0613     '* AREA DO ARQUIVO DE NULIDADE RUECB09H *'.
AC0613*----------------------------------------------------------------*
AC0613
AC0613 01  WRK-RUEC09HS-NULL.
AC0613     05  WRK-B09H-CPRODT-NULL       PIC  X(01)      VALUE SPACES.
AC0613     05  WRK-B09H-DULT-NULL         PIC  X(01)      VALUE SPACES.
AC0613     05  WRK-B09H-CUSUAR-NULL       PIC  X(01)      VALUE SPACES.
AC0613
AC0613*----------------------------------------------------------------*
AC0613 01  FILLER                      PIC  X(42) VALUE
AC0613     '* AREA DO ARQUIVO DE NULIDADE RUECB09I *'.
AC0613*----------------------------------------------------------------*
AC0613
AC0613 01  WRK-RUEC09IS-NULL.
AC0613     05  WRK-B09I-DSELEC-NULL        PIC  X(01)      VALUE SPACES.
AC0613     05  WRK-B09I-NPCELA-NULL        PIC  X(01)      VALUE SPACES.
AC0613     05  WRK-B09I-CMODLD-NULL        PIC  X(01)      VALUE SPACES.
AC0613     05  WRK-B09I-VRJURO-NULL        PIC  X(01)      VALUE SPACES.
AC0613     05  WRK-B09I-VPCELA-NULL        PIC  X(01)      VALUE SPACES.
AC0613     05  WRK-B09I-VRESGT-NULL        PIC  X(01)      VALUE SPACES.
AC0613     05  WRK-B09I-VSDO-NULL          PIC  X(01)      VALUE SPACES.
AC0613     05  WRK-B09I-CSELEC-NULL        PIC  X(01)      VALUE SPACES.
AC0613     05  WRK-B09I-DREG-NULL          PIC  X(01)      VALUE SPACES.
AC0613     05  WRK-B09I-CREG-NULL          PIC  X(01)      VALUE SPACES.
AC0613     05  WRK-B09I-CCMARA-NULL        PIC  X(01)      VALUE SPACES.
AC0613     05  WRK-B09I-NSERIE-NULL        PIC  X(01)      VALUE SPACES.
AC0613     05  WRK-B09I-CCONTR-NULL        PIC  X(01)      VALUE SPACES.
AC0613     05  WRK-B09I-CSIT-NULL          PIC  X(01)      VALUE SPACES.
AC0613     05  WRK-B09I-CJUNC-NULL         PIC  X(01)      VALUE SPACES.
AC0613     05  WRK-B09I-CSECAO-NULL        PIC  X(01)      VALUE SPACES.
AC0613     05  WRK-B09I-CUSUAR-NULL        PIC  X(01)      VALUE SPACES.
AC0613     05  WRK-B09I-RMOTVO-NULL        PIC  X(01)      VALUE SPACES.
AC0613     05  WRK-B09I-RESP-NULL          PIC  X(01)      VALUE SPACES.
AC0613     05  WRK-B09I-DEXCL-NULL         PIC  X(01)      VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(38) VALUE
           '* INDICADORES DE NULIDADE RUECB008 *'.
      *----------------------------------------------------------------*

       01  WRK-RUEC008S-N.
           05  WRK-B008-VRESP-RURAL-N
                                           PIC S9(04) COMP  VALUE ZEROS.
           05  WRK-B008-VRESP-GRAL-N
                                           PIC S9(04) COMP  VALUE ZEROS.
           05  WRK-B008-VSDO-MED-TMES-N
                                           PIC S9(04) COMP  VALUE ZEROS.
           05  WRK-B008-VSDO-MED-SEMTL-N
                                           PIC S9(04) COMP  VALUE ZEROS.
           05  WRK-B008-VRENDA-AGROP-OUTRO-N
                                           PIC S9(04) COMP  VALUE ZEROS.
           05  WRK-B008-VRENDA-NAO-AGROP-N
                                           PIC S9(04) COMP  VALUE ZEROS.
           05  WRK-B008-DCAD-N
                                           PIC S9(04) COMP  VALUE ZEROS.
           05  WRK-B008-DANO-RENDM-N
                                           PIC S9(04) COMP  VALUE ZEROS.
           05  WRK-B008-CSEQ-HIPOT-RURAL-N
                                           PIC S9(04) COMP  VALUE ZEROS.
           05  WRK-B008-CTPO-DOCTO-PSSOA-N
                                           PIC S9(04) COMP  VALUE ZEROS.
           05  WRK-B008-CDOCTO-PSSOA-N
                                           PIC S9(04) COMP  VALUE ZEROS.
           05  WRK-B008-CORG-EMISR-DOCTO-N
                                           PIC S9(04) COMP  VALUE ZEROS.
           05  WRK-B008-CUND-ORG-EMISR-N
                                           PIC S9(04) COMP  VALUE ZEROS.
           05  WRK-B008-CSEXO-N
                                           PIC S9(04) COMP  VALUE ZEROS.
           05  WRK-B008-CEST-CVIL-RGIME-N
                                           PIC S9(04) COMP  VALUE ZEROS.
           05  WRK-B008-DEST-CVIL-N
                                           PIC S9(04) COMP  VALUE ZEROS.
           05  WRK-B008-CNAC-N
                                           PIC S9(04) COMP  VALUE ZEROS.
           05  WRK-B008-CSGL-UF-N
                                           PIC S9(04) COMP  VALUE ZEROS.
           05  WRK-B008-INAT-ESTRG-N
                                           PIC S9(04) COMP  VALUE ZEROS.
           05  WRK-B008-ECOMPL-LOGDR-N
                                           PIC S9(04) COMP  VALUE ZEROS.
           05  WRK-B008-CCATEG-PROFS-CREDT-N
                                           PIC S9(04) COMP  VALUE ZEROS.
           05  WRK-B008-CPROFS-PSSOA-N
                                           PIC S9(04) COMP  VALUE ZEROS.
           05  WRK-B008-CCLASS-ATVDD-ECONC-N
                                           PIC S9(04) COMP  VALUE ZEROS.
           05  WRK-B008-CRAMO-ATVDD-ECONC-N
                                           PIC S9(04) COMP  VALUE ZEROS.
           05  WRK-B008-CSRAMO-ATVDD-ECONC-N
                                           PIC S9(04) COMP  VALUE ZEROS.
           05  WRK-B008-CATVDD-ECONC-N
                                           PIC S9(04) COMP  VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(38) VALUE
           '* INDICADORES DE NULIDADE RUECB009 *'.
      *----------------------------------------------------------------*

       01  WRK-RUEC009S-N.
           05  WRK-B009-RCOMPL-QUALF-GARNT-N
                                           PIC S9(04) COMP  VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(38) VALUE
           '* INDICADORES DE NULIDADE RUECV010 *'.
      *----------------------------------------------------------------*

       01  WRK-RUEC010S-N.
           05  WRK-V010-VPCELA-REC-PPRIO-N
                                           PIC S9(04) COMP  VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(38) VALUE
           '* INDICADORES DE NULIDADE RUECB014 *'.
      *----------------------------------------------------------------*

       01  WRK-RUEC014S-N.
           05  WRK-B014-VRESP-RURAL-N
                                           PIC S9(04) COMP  VALUE ZEROS.
           05  WRK-B014-VRESP-GRAL-N
                                           PIC S9(04) COMP  VALUE ZEROS.
           05  WRK-B014-VSDO-MED-TMES-N
                                           PIC S9(04) COMP  VALUE ZEROS.
           05  WRK-B014-VSDO-MED-SEMTL-N
                                           PIC S9(04) COMP  VALUE ZEROS.
           05  WRK-B014-VRENDA-AGROP-OUTRO-N
                                           PIC S9(04) COMP  VALUE ZEROS.
           05  WRK-B014-VRENDA-NAO-AGROP-N
                                           PIC S9(04) COMP  VALUE ZEROS.
           05  WRK-B014-DCAD-N
                                           PIC S9(04) COMP  VALUE ZEROS.
           05  WRK-B014-DANO-RENDM-N
                                           PIC S9(04) COMP  VALUE ZEROS.
           05  WRK-B014-CSEQ-HIPOT-RURAL-N
                                           PIC S9(04) COMP  VALUE ZEROS.
           05  WRK-B014-CTPO-DOCTO-PSSOA-N
                                           PIC S9(04) COMP  VALUE ZEROS.
           05  WRK-B014-CDOCTO-PSSOA-N
                                           PIC S9(04) COMP  VALUE ZEROS.
           05  WRK-B014-CORG-EMISR-DOCTO-N
                                           PIC S9(04) COMP  VALUE ZEROS.
           05  WRK-B014-CUND-ORG-EMISR-N
                                           PIC S9(04) COMP  VALUE ZEROS.
           05  WRK-B014-CSEXO-N
                                           PIC S9(04) COMP  VALUE ZEROS.
           05  WRK-B014-CEST-CVIL-RGIME-N
                                           PIC S9(04) COMP  VALUE ZEROS.
           05  WRK-B014-DEST-CVIL-N
                                           PIC S9(04) COMP  VALUE ZEROS.
           05  WRK-B014-CNAC-N
                                           PIC S9(04) COMP  VALUE ZEROS.
           05  WRK-B014-CSGL-UF-N
                                           PIC S9(04) COMP  VALUE ZEROS.
           05  WRK-B014-INAT-ESTRG-N
                                           PIC S9(04) COMP  VALUE ZEROS.
           05  WRK-B014-ECOMPL-LOGDR-N
                                           PIC S9(04) COMP  VALUE ZEROS.
           05  WRK-B014-CCATEG-PROFS-CREDT-N
                                           PIC S9(04) COMP  VALUE ZEROS.
           05  WRK-B014-CPROFS-PSSOA-N
                                           PIC S9(04) COMP  VALUE ZEROS.
           05  WRK-B014-CCLASS-ATVDD-ECONC-N
                                           PIC S9(04) COMP  VALUE ZEROS.
           05  WRK-B014-CRAMO-ATVDD-ECONC-N
                                           PIC S9(04) COMP  VALUE ZEROS.
           05  WRK-B014-CSRAMO-ATVDD-ECONC-N
                                           PIC S9(04) COMP  VALUE ZEROS.
           05  WRK-B014-CATVDD-ECONC-N
                                           PIC S9(04) COMP  VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(38) VALUE
           '* INDICADORES DE NULIDADE RUECB017 *'.
      *----------------------------------------------------------------*

       01  WRK-RUEC017S-N.
           05  WRK-B017-VAVALC-IMOV-N
                                           PIC S9(04) COMP  VALUE ZEROS.
           05  WRK-B017-CSEQ-ESCRT-ONUS-N
                                           PIC S9(04) COMP  VALUE ZEROS.
           05  WRK-B017-CSEQ-PATRM-IMOV-N
                                           PIC S9(04) COMP  VALUE ZEROS.
           05  WRK-B017-CCARTR-IMOV-REG-N
                                           PIC S9(04) COMP  VALUE ZEROS.
           05  WRK-B017-CGLEBA-IMOV-HIPOT-N
                                           PIC S9(04) COMP  VALUE ZEROS.
           05  WRK-B017-CLOTE-IMOV-HIPOT-N
                                           PIC S9(04) COMP  VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(38) VALUE
           '* INDICADORES DE NULIDADE RUECV022 *'.
      *----------------------------------------------------------------*

       01  WRK-RUEC022S-N.
           05  WRK-V022-DLIBRC-OPER-ORCAM-N
                                           PIC S9(04) COMP  VALUE ZEROS.
           05  WRK-V022-DFIM-OPER-ORCAM-N  PIC S9(04) COMP  VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(38) VALUE
           '* INDICADORES DE NULIDADE RUECV025 *'.
      *----------------------------------------------------------------*

       01  WRK-RUEC025S-N.
           05  WRK-V025-DLIBRC-OPER-ORCAM-N
                                           PIC S9(04) COMP  VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(38) VALUE
           '* INDICADORES DE NULIDADE RUECB035 *'.
      *----------------------------------------------------------------*

       01  WRK-RUEC035S-N.
           05  WRK-B035-CSEQ-ESCRT-ONUS-N
                                           PIC S9(04) COMP  VALUE ZEROS.

           05  WRK-B035-CSEQ-PATRM-IMOV-N
                                           PIC S9(04) COMP  VALUE ZEROS.
           05  WRK-B035-CCARTR-IMOV-REG-N
                                           PIC S9(04) COMP  VALUE ZEROS.
           05  WRK-B035-CGLEBA-IMOV-HIPOT-N
                                           PIC S9(04) COMP  VALUE ZEROS.
           05  WRK-B035-CLOTE-IMOV-HIPOT-N
                                           PIC S9(04) COMP  VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(38) VALUE
           '* INDICADORES DE NULIDADE RUECB040 *'.
      *----------------------------------------------------------------*

       01  WRK-RUEC040S-N.
           05 WRK-B040-CID-N                PIC S9(04) COMP VALUE +0.
           05 WRK-B040-MLATTD-N             PIC S9(04) COMP VALUE +0.
           05 WRK-B040-MLOGTD-N             PIC S9(04) COMP VALUE +0.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(38) VALUE
           '* INDICADORES DE NULIDADE RUECB041 *'.
      *----------------------------------------------------------------*

       01  WRK-RUEC041S-N.
           05 WRK-B041-CID-N                PIC S9(04) COMP VALUE +0.
           05 WRK-B041-MLATTD-N             PIC S9(04) COMP VALUE +0.
           05 WRK-B041-MLOGTD-N             PIC S9(04) COMP VALUE +0.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(38) VALUE
           '* INDICADORES DE NULIDADE RUECB044 *'.
      *----------------------------------------------------------------*

       01  WRK-RUEC044S-N.
           05  WRK-B044-HULT-ATULZ-N
                                           PIC S9(04) COMP  VALUE ZEROS.
           05  WRK-B044-CFUNC-ULT-ATULZ-N
                                           PIC S9(04) COMP  VALUE ZEROS.


      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(38) VALUE
           '* INDICADORES DE NULIDADE RUECB052 *'.
      *----------------------------------------------------------------*

       01  WRK-RUEC052S-N.
           05  WRK-B052-CMUN-RURAL-N
                                           PIC S9(04) COMP  VALUE ZEROS.
           05  WRK-B052-CCEP-N
                                           PIC S9(04) COMP  VALUE ZEROS.
           05  WRK-B052-CCEP-COMPL-N
                                           PIC S9(04) COMP  VALUE ZEROS.
           05  WRK-B052-RTPO-IMOV-ARREN-N
                                           PIC S9(04) COMP  VALUE ZEROS.
           05  WRK-B052-RTPO-DOCTO-N
                                           PIC S9(04) COMP  VALUE ZEROS.
           05  WRK-B052-CCARTR-IMOV-REG-N
                                           PIC S9(04) COMP  VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(38) VALUE
           '* INDICADORES DE NULIDADE RUECB064 *'.
      *----------------------------------------------------------------*

       01  WRK-RUEC064S-N.
           05  WRK-B064-VFINAN-N
                                           PIC S9(04) COMP  VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(38) VALUE
           '* INDICADORES DE NULIDADE RUECB065 *'.
      *----------------------------------------------------------------*

       01  WRK-RUEC065S-N.
           05  WRK-B065-CREG-PNHOR-RURAL-N
                                           PIC S9(04) COMP  VALUE ZEROS.
           05  WRK-B065-CLIVRO-PNHOR-RURAL-N
                                           PIC S9(04) COMP  VALUE ZEROS.
           05  WRK-B065-DREG-PNHOR-RURAL-N
                                           PIC S9(04) COMP  VALUE ZEROS.
           05  WRK-B065-CREG-HIPOT-RURAL-N
                                           PIC S9(04) COMP  VALUE ZEROS.
           05  WRK-B065-CLIVRO-HIPOT-RURAL-N
                                           PIC S9(04) COMP  VALUE ZEROS.
           05  WRK-B065-DREG-HIPOT-RURAL-N
                                           PIC S9(04) COMP  VALUE ZEROS.
           05  WRK-B065-DINCL-CARTR-RURAL-N
                                           PIC S9(04) COMP  VALUE ZEROS.
           05  WRK-B065-HULT-ATULZ-N
                                           PIC S9(04) COMP  VALUE ZEROS.
           05  WRK-B065-CFUNC-ULT-ATULZ-N
                                           PIC S9(04) COMP  VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(38) VALUE
           '* INDICADORES DE NULIDADE RUECB066 *'.
      *----------------------------------------------------------------*

       01  WRK-RUEC066S-N.
           05  WRK-B066-DINCL-OBS-LICEN-N
                                           PIC S9(04) COMP  VALUE ZEROS.
           05  WRK-B066-HULT-ATULZ-N
                                           PIC S9(04) COMP  VALUE ZEROS.
           05  WRK-B066-CFUNC-ULT-ATULZ-N
                                           PIC S9(04) COMP  VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(38) VALUE
           '* INDICADORES DE NULIDADE RUECB074 *'.
      *----------------------------------------------------------------*

       01  WRK-RUEC074S-N.
           05  WRK-B074-CORG-EMISR-DOCTO-N
                                           PIC S9(04) COMP  VALUE ZEROS.
           05  WRK-B074-CUND-ORG-EMISR-N
                                           PIC S9(04) COMP  VALUE ZEROS.
           05  WRK-B074-CSGL-UF-N
                                           PIC S9(04) COMP  VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(38) VALUE
           '* INDICADORES DE NULIDADE RUECB075 *'.
      *----------------------------------------------------------------*

       01  WRK-RUEC075S-N.
           05  WRK-B075-CORG-EMISR-DOCTO-N
                                           PIC S9(04) COMP  VALUE ZEROS.
           05  WRK-B075-CUND-ORG-EMISR-N
                                           PIC S9(04) COMP  VALUE ZEROS.
           05  WRK-B075-CSGL-UF-N
                                           PIC S9(04) COMP  VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(38) VALUE
           '* INDICADORES DE NULIDADE RUECB096 *'.
      *----------------------------------------------------------------*

       01  WRK-RUEC096S-N.
           05  WRK-B096-CINDCD-SIT-CDULA-N
                                           PIC S9(04) COMP  VALUE ZEROS.
           05  WRK-B096-CREFT-BACEN-N
                                           PIC S9(04) COMP  VALUE ZEROS.

AC0613*----------------------------------------------------------------*
AC0613 01  FILLER                      PIC  X(38) VALUE
AC0613     '* INDICADORES DE NULIDADE RUECB097 *'.
AC0613*----------------------------------------------------------------*
AC0613
AC0613 01  WRK-RUEC097S-N.
AC0613     05  WRK-B097-CUSUAR-N      PIC S9(04) COMP  VALUE ZEROS.
AC0613     05  WRK-B097-CMODLD-N      PIC S9(04) COMP  VALUE ZEROS.
AC0613     05  WRK-B097-CPRODT-N      PIC S9(04) COMP  VALUE ZEROS.
AC0613     05  WRK-B097-CESTCA-N      PIC S9(04) COMP  VALUE ZEROS.

AC0613*----------------------------------------------------------------*
AC0613 01  FILLER                      PIC  X(38) VALUE
AC0613     '* INDICADORES DE NULIDADE RUECB099 *'.
AC0613*----------------------------------------------------------------*
AC0613
AC0613 01  WRK-RUEC099S-N.
AC0613     05  WRK-B099-DCDULA-N           PIC S9(04) COMP  VALUE ZEROS.
AC0613     05  WRK-B099-DVCTO-N            PIC S9(04) COMP  VALUE ZEROS.
AC0613     05  WRK-B099-VFINAN-N           PIC S9(04) COMP  VALUE ZEROS.
AC0613     05  WRK-B099-CCATEG-N           PIC S9(04) COMP  VALUE ZEROS.
AC0613     05  WRK-B099-CPROG-N            PIC S9(04) COMP  VALUE ZEROS.
AC0613     05  WRK-B099-CMUN-N             PIC S9(04) COMP  VALUE ZEROS.
AC0613     05  WRK-B099-LIBRC-N            PIC S9(04) COMP  VALUE ZEROS.
AC0613     05  WRK-B099-REC-N              PIC S9(04) COMP  VALUE ZEROS.
AC0613     05  WRK-B099-FINCD-N            PIC S9(04) COMP  VALUE ZEROS.
AC0613     05  WRK-B099-PROD-N             PIC S9(04) COMP  VALUE ZEROS.
AC0613     05  WRK-B099-DINIC-N            PIC S9(04) COMP  VALUE ZEROS.
AC0613     05  WRK-B099-DFIM-N             PIC S9(04) COMP  VALUE ZEROS.
AC0613     05  WRK-B099-GARNT-N            PIC S9(04) COMP  VALUE ZEROS.
AC0613
AC0613*----------------------------------------------------------------*
AC0613 01  FILLER                      PIC  X(38) VALUE
AC0613     '* INDICADORES DE NULIDADE RUECB09A *'.
AC0613*----------------------------------------------------------------*
AC0613
AC0613 01  WRK-RUEC09AS-N.
AC0613     05  WRK-B09A-CSIT-N             PIC S9(04) COMP  VALUE ZEROS.
AC0613     05  WRK-B09A-HINCL-N            PIC S9(04) COMP  VALUE ZEROS.
AC0613
AC0613*----------------------------------------------------------------*
AC0613 01  FILLER                      PIC  X(38) VALUE
AC0613     '* INDICADORES DE NULIDADE RUECB09B *'.
AC0613*----------------------------------------------------------------*
AC0613
AC0613 01  WRK-RUEC09BS-N.
AC0613     05  WRK-B09B-CMSGEM-N           PIC S9(04) COMP  VALUE ZEROS.
AC0613     05  WRK-B09B-HINCL-N            PIC S9(04) COMP  VALUE ZEROS.
AC0613     05  WRK-B09B-CCTRL-N            PIC S9(04) COMP  VALUE ZEROS.
AC0613     05  WRK-B09B-CUNIC-N            PIC S9(04) COMP  VALUE ZEROS.
AC0613     05  WRK-B09B-CREFT-N            PIC S9(04) COMP  VALUE ZEROS.
AC0613     05  WRK-B09B-CSIT-N             PIC S9(04) COMP  VALUE ZEROS.
AC0613     05  WRK-B09B-CUSUAR-N           PIC S9(04) COMP  VALUE ZEROS.
AC0613
AC0613*----------------------------------------------------------------*
AC0613 01  FILLER                      PIC  X(38) VALUE
AC0613     '* INDICADORES DE NULIDADE RUECB09D *'.
AC0613*----------------------------------------------------------------*
AC0613
AC0613 01  WRK-RUEC09DS-N.
AC0613     05  WRK-B09D-DVCTO-N            PIC S9(04) COMP  VALUE ZEROS.
AC0613
AC0613*----------------------------------------------------------------*
AC0613 01  FILLER                      PIC  X(38) VALUE
AC0613     '* INDICADORES DE NULIDADE RUECB09E *'.
AC0613*----------------------------------------------------------------*
AC0613
AC0613 01  WRK-RUEC09ES-N.
AC0613     05  WRK-B09E-RESP-N             PIC S9(04) COMP  VALUE ZEROS.
AC0613     05  WRK-B09E-PSSOA-N            PIC S9(04) COMP  VALUE ZEROS.
AC0613     05  WRK-B09E-CCNPJ-N            PIC S9(04) COMP  VALUE ZEROS.
AC0613     05  WRK-B09E-CFLIAL-N           PIC S9(04) COMP  VALUE ZEROS.
AC0613     05  WRK-B09E-CCTRL-N            PIC S9(04) COMP  VALUE ZEROS.
AC0613
AC0613*----------------------------------------------------------------*
AC0613 01  FILLER                      PIC  X(38) VALUE
AC0613     '* INDICADORES DE NULIDADE RUECB09F *'.
AC0613*----------------------------------------------------------------*
AC0613
AC0613 01  WRK-RUEC09FS-N.
AC0613     05  WRK-B09F-CPRODT-N           PIC S9(04) COMP  VALUE ZEROS.
AC0613
AC0613*----------------------------------------------------------------*
AC0613 01  FILLER                      PIC  X(38) VALUE
AC0613     '* INDICADORES DE NULIDADE RUECB09G *'.
AC0613*----------------------------------------------------------------*
AC0613
AC0613 01  WRK-RUEC09GS-N.
AC0613     05  WRK-B09G-CPRODT-N           PIC S9(04) COMP  VALUE ZEROS.
AC0613     05  WRK-B09G-DULT-N             PIC S9(04) COMP  VALUE ZEROS.
AC0613     05  WRK-B09G-CUSUAR-N           PIC S9(04) COMP  VALUE ZEROS.
AC0613
AC0613*----------------------------------------------------------------*
AC0613 01  FILLER                      PIC  X(38) VALUE
AC0613     '* INDICADORES DE NULIDADE RUECB09H *'.
AC0613*----------------------------------------------------------------*
AC0613
AC0613 01  WRK-RUEC09H-N.
AC0613     05  WRK-B09H-CPRODT-N          PIC S9(04) COMP  VALUE ZEROS.
AC0613     05  WRK-B09H-DULT-N            PIC S9(04) COMP  VALUE ZEROS.
AC0613     05  WRK-B09H-CUSUAR-N          PIC S9(04) COMP  VALUE ZEROS.
AC0613
AC0613*----------------------------------------------------------------*
AC0613 01  FILLER                      PIC  X(38) VALUE
AC0613     '* INDICADORES DE NULIDADE RUECB09I *'.
AC0613*----------------------------------------------------------------*
AC0613
AC0613 01  WRK-RUEC09IS-N.
AC0613     05  WRK-B09I-DSELEC-N           PIC S9(04) COMP  VALUE ZEROS.
AC0613     05  WRK-B09I-NPCELA-N           PIC S9(04) COMP  VALUE ZEROS.
AC0613     05  WRK-B09I-CMODLD-N           PIC S9(04) COMP  VALUE ZEROS.
AC0613     05  WRK-B09I-VRJURO-N           PIC S9(04) COMP  VALUE ZEROS.
AC0613     05  WRK-B09I-VPCELA-N           PIC S9(04) COMP  VALUE ZEROS.
AC0613     05  WRK-B09I-VRESGT-N           PIC S9(04) COMP  VALUE ZEROS.
AC0613     05  WRK-B09I-VSDO-N             PIC S9(04) COMP  VALUE ZEROS.
AC0613     05  WRK-B09I-CSELEC-N           PIC S9(04) COMP  VALUE ZEROS.
AC0613     05  WRK-B09I-DREG-N             PIC S9(04) COMP  VALUE ZEROS.
AC0613     05  WRK-B09I-CREG-N             PIC S9(04) COMP  VALUE ZEROS.
AC0613     05  WRK-B09I-CCMARA-N           PIC S9(04) COMP  VALUE ZEROS.
AC0613     05  WRK-B09I-NSERIE-N           PIC S9(04) COMP  VALUE ZEROS.
AC0613     05  WRK-B09I-CCONTR-N           PIC S9(04) COMP  VALUE ZEROS.
AC0613     05  WRK-B09I-CSIT-N             PIC S9(04) COMP  VALUE ZEROS.
AC0613     05  WRK-B09I-CJUNC-N            PIC S9(04) COMP  VALUE ZEROS.
AC0613     05  WRK-B09I-CSECAO-N           PIC S9(04) COMP  VALUE ZEROS.
AC0613     05  WRK-B09I-CUSUAR-N           PIC S9(04) COMP  VALUE ZEROS.
AC0613     05  WRK-B09I-RMOTVO-N           PIC S9(04) COMP  VALUE ZEROS.
AC0613     05  WRK-B09I-RESP-N             PIC S9(04) COMP  VALUE ZEROS.
AC0613     05  WRK-B09I-DEXCL-N            PIC S9(04) COMP  VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32) VALUE
           '* AREA PARA DISPLAYS DE TOTAIS *'.
      *----------------------------------------------------------------*

       COPY 'I#RURAYY'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32) VALUE
           '* AREA DA BRAD7100             *'.
      *----------------------------------------------------------------*
       COPY 'I#BRAD7C'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32) VALUE
           '* INCLUDE RUECB008             *'.
      *----------------------------------------------------------------*

           EXEC SQL
               INCLUDE RUECB008
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32) VALUE
           '* INCLUDE RUECB009             *'.
      *----------------------------------------------------------------*

           EXEC SQL
               INCLUDE RUECB009
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32) VALUE
           '* INCLUDE RUECV010             *'.
      *----------------------------------------------------------------*

           EXEC SQL
               INCLUDE RUECV010
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32) VALUE
           '* INCLUDE RUECV011             *'.
      *----------------------------------------------------------------*

           EXEC SQL
               INCLUDE RUECV011
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32) VALUE
           '* INCLUDE RUECV012             *'.
      *----------------------------------------------------------------*

           EXEC SQL
               INCLUDE RUECV012
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32) VALUE
           '* INCLUDE RUECB014             *'.
      *----------------------------------------------------------------*

           EXEC SQL
               INCLUDE RUECB014
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32) VALUE
           '* INCLUDE RUECB017             *'.
      *----------------------------------------------------------------*

           EXEC SQL
               INCLUDE RUECB017
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32) VALUE
           '* INCLUDE RUECV018             *'.
      *----------------------------------------------------------------*

           EXEC SQL
               INCLUDE RUECV018
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32) VALUE
           '* INCLUDE RUECV019             *'.
      *----------------------------------------------------------------*

           EXEC SQL
               INCLUDE RUECV019
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32) VALUE
           '* INCLUDE RUECV022             *'.
      *----------------------------------------------------------------*

           EXEC SQL
               INCLUDE RUECV022
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32) VALUE
           '* INCLUDE RUECV023             *'.
      *----------------------------------------------------------------*

           EXEC SQL
               INCLUDE RUECV023
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32) VALUE
           '* INCLUDE RUECB024             *'.
      *----------------------------------------------------------------*

           EXEC SQL
               INCLUDE RUECB024
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32) VALUE
           '* INCLUDE RUECV025             *'.
      *----------------------------------------------------------------*

           EXEC SQL
               INCLUDE RUECV025
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32) VALUE
           '* INCLUDE RUECV026             *'.
      *----------------------------------------------------------------*

           EXEC SQL
               INCLUDE RUECV026
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32) VALUE
           '* INCLUDE RUECB027             *'.
      *----------------------------------------------------------------*

           EXEC SQL
               INCLUDE RUECB027
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32) VALUE
           '* INCLUDE RUECB028             *'.
      *----------------------------------------------------------------*

           EXEC SQL
               INCLUDE RUECB028
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32) VALUE
           '* INCLUDE RUECB033             *'.
      *----------------------------------------------------------------*

           EXEC SQL
               INCLUDE RUECB033
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32) VALUE
           '* INCLUDE RUECB034             *'.
      *----------------------------------------------------------------*

           EXEC SQL
               INCLUDE RUECB034
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32) VALUE
           '* INCLUDE RUECB035             *'.
      *----------------------------------------------------------------*

           EXEC SQL
               INCLUDE RUECB035
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32) VALUE
           '* INCLUDE RUECB036             *'.
      *----------------------------------------------------------------*

           EXEC SQL
               INCLUDE RUECB036
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32) VALUE
           '* INCLUDE RUECB038             *'.
      *----------------------------------------------------------------*

           EXEC SQL
               INCLUDE RUECB038
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32) VALUE
           '* INCLUDE RUECB040             *'.
      *----------------------------------------------------------------*

           EXEC SQL
               INCLUDE RUECB040
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32) VALUE
           '* INCLUDE RUECB041             *'.
      *----------------------------------------------------------------*

           EXEC SQL
               INCLUDE RUECB041
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32) VALUE
           '* INCLUDE RUECB044             *'.
      *----------------------------------------------------------------*

           EXEC SQL
               INCLUDE RUECB044
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32) VALUE
           '* INCLUDE RUECB052             *'.
      *----------------------------------------------------------------*

           EXEC SQL
               INCLUDE RUECB052
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32) VALUE
           '* INCLUDE RUECB053             *'.
      *----------------------------------------------------------------*

           EXEC SQL
               INCLUDE RUECB053
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32) VALUE
           '* INCLUDE RUECB054             *'.
      *----------------------------------------------------------------*

           EXEC SQL
               INCLUDE RUECB054
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32) VALUE
           '* INCLUDE RUECB064             *'.
      *----------------------------------------------------------------*

           EXEC SQL
               INCLUDE RUECB064
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32) VALUE
           '* INCLUDE RUECB065             *'.
      *----------------------------------------------------------------*

           EXEC SQL
               INCLUDE RUECB065
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32) VALUE
           '* INCLUDE RUECB066             *'.
      *----------------------------------------------------------------*

           EXEC SQL
               INCLUDE RUECB066
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32) VALUE
           '* INCLUDE RUECB073             *'.
      *----------------------------------------------------------------*

           EXEC SQL
               INCLUDE RUECB073
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32) VALUE
           '* INCLUDE RUECB074             *'.
      *----------------------------------------------------------------*

           EXEC SQL
               INCLUDE RUECB074
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32) VALUE
           '* INCLUDE RUECB075             *'.
      *----------------------------------------------------------------*

           EXEC SQL
               INCLUDE RUECB075
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32) VALUE
           '* INCLUDE RUECB096             *'.
      *----------------------------------------------------------------*

           EXEC SQL
               INCLUDE RUECB096
           END-EXEC.

AC0613*----------------------------------------------------------------*
AC0613 01  FILLER                      PIC  X(32) VALUE
AC0613     '* INCLUDE RUECB097             *'.
AC0613*----------------------------------------------------------------*
AC0613
AC0613     EXEC SQL
AC0613         INCLUDE RUECB097
AC0613     END-EXEC.
AC0613
AC0613*----------------------------------------------------------------*
AC0613 01  FILLER                      PIC  X(32) VALUE
AC0613     '* INCLUDE RUECB099             *'.
AC0613*----------------------------------------------------------------*
AC0613
AC0613     EXEC SQL
AC0613         INCLUDE RUECB099
AC0613     END-EXEC.
AC0613
AC0613*----------------------------------------------------------------*
AC0613 01  FILLER                      PIC  X(32) VALUE
AC0613     '* INCLUDE RUECB09A             *'.
AC0613*----------------------------------------------------------------*
AC0613
AC0613     EXEC SQL
AC0613         INCLUDE RUECB09A
AC0613     END-EXEC.
AC0613
AC0613*----------------------------------------------------------------*
AC0613 01  FILLER                      PIC  X(32) VALUE
AC0613     '* INCLUDE RUECB09B             *'.
AC0613*----------------------------------------------------------------*
AC0613
AC0613     EXEC SQL
AC0613         INCLUDE RUECB09B
AC0613     END-EXEC.
AC0613
AC0613*----------------------------------------------------------------*
AC0613 01  FILLER                      PIC  X(32) VALUE
AC0613     '* INCLUDE RUECB09D             *'.
AC0613*----------------------------------------------------------------*
AC0613
AC0613     EXEC SQL
AC0613         INCLUDE RUECB09D
AC0613     END-EXEC.
AC0613
AC0613*----------------------------------------------------------------*
AC0613 01  FILLER                      PIC  X(32) VALUE
AC0613     '* INCLUDE RUECB09E             *'.
AC0613*----------------------------------------------------------------*
AC0613
AC0613     EXEC SQL
AC0613         INCLUDE RUECB09E
AC0613     END-EXEC.
AC0613
AC0613*----------------------------------------------------------------*
AC0613 01  FILLER                      PIC  X(32) VALUE
AC0613     '* INCLUDE RUECB09F             *'.
AC0613*----------------------------------------------------------------*
AC0613
AC0613     EXEC SQL
AC0613         INCLUDE RUECB09F
AC0613     END-EXEC.
AC0613
AC0613*----------------------------------------------------------------*
AC0613 01  FILLER                      PIC  X(32) VALUE
AC0613     '* INCLUDE RUECB09G             *'.
AC0613*----------------------------------------------------------------*
AC0613
AC0613     EXEC SQL
AC0613         INCLUDE RUECB09G
AC0613     END-EXEC.
AC0613
AC0613*----------------------------------------------------------------*
AC0613 01  FILLER                      PIC  X(32) VALUE
AC0613     '* INCLUDE RUECB09H             *'.
AC0613*----------------------------------------------------------------*
AC0613
AC0613     EXEC SQL
AC0613         INCLUDE RUECB09H
AC0613     END-EXEC.
AC0613
AC0613*----------------------------------------------------------------*
AC0613 01  FILLER                      PIC  X(32) VALUE
AC0613     '* INCLUDE RUECB09I             *'.
AC0613*----------------------------------------------------------------*
AC0613
AC0613     EXEC SQL
AC0613         INCLUDE RUECB09I
AC0613     END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32) VALUE
           '* INCLUDE SQLCA                *'.
      *----------------------------------------------------------------*

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32) VALUE
           '* DECLARACAO DE CURSORES       *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    CSR01-RUECB008 - SELECAO DE CEDULAS                         *
      *----------------------------------------------------------------*

           EXEC SQL
             DECLARE CSR01-RUECB008 CURSOR FOR
           SELECT
             CJUNC_DEPDC
           , CCDULA
           , CSEQ_PSSOA
           , CTPO_RESP_PSSOA
           , IPSSOA
           , CTPO_PSSOA
           , CCGC_CPF
           , CFLIAL_CGC
           , CCTRL_CPF_CGC
           , VRESP_RURAL
           , VRESP_GRAL
           , VSDO_MED_TMES
           , VSDO_MED_SEMTL
           , VRENDA_AGROP_OUTRO
           , VRENDA_NAO_AGROP
           , DCAD
           , DANO_RENDM
           , CSEQ_HIPOT_RURAL
           , CTPO_DOCTO_PSSOA
           , CDOCTO_PSSOA
           , CORG_EMISR_DOCTO
           , CUND_ORG_EMISR
           , CSEXO
           , CEST_CVIL_RGIME
           , DEST_CVIL
           , CNAC
           , CSGL_UF
           , INAT_ESTRG
           , ELOGDR
           , ENRO_LOGDR
           , ECOMPL_LOGDR
           , EBAIRO_LOGDR
           , CCEP
           , CCEP_COMPL
           , CMUN_RURAL
           , CCATEG_PROFS_CREDT
           , CPROFS_PSSOA
           , CCLASS_ATVDD_ECONC
           , CRAMO_ATVDD_ECONC
           , CSRAMO_ATVDD_ECONC
           , CATVDD_ECONC
           FROM  DB2PRD.PSSOA_CDULA_TEMPR
           WHERE  CJUNC_DEPDC = :WRK-B007-CJUNC-DEPDC
           AND    CCDULA      = :WRK-B007-CCDULA
           ORDER BY
             CSEQ_PSSOA
           END-EXEC.


      *----------------------------------------------------------------*
      *    CSR02-RUECB009 - SELECAO DE CEDULAS                         *
      *----------------------------------------------------------------*

           EXEC SQL
             DECLARE CSR02-RUECB009 CURSOR FOR
           SELECT
             CJUNC_DEPDC
           , CCDULA
           , CSEQ_GARNT
           , RGARNT_CREDT_RURAL
           , QGARNT
           , RQUALF_GARNT
           , RCOMPL_QUALF_GARNT
           , VGARNT
           , ILOC_GARNT
           , CMUN_RURAL
           FROM  DB2PRD.GARNT_CDULA_TEMPR
           WHERE  CJUNC_DEPDC = :WRK-B007-CJUNC-DEPDC
           AND    CCDULA      = :WRK-B007-CCDULA
           ORDER BY
             CSEQ_GARNT
           END-EXEC.


      *----------------------------------------------------------------*
      *    CSR03-RUECV010 - SELECAO DE CEDULAS                         *
      *----------------------------------------------------------------*

           EXEC SQL
             DECLARE CSR03-RUECV010 CURSOR FOR
           SELECT
             CJUNC_DEPDC
           , CCDULA
           , CTPO_EVNTO_RURAL
           , DEVNTO
           , VEVNTO_RURAL
           , VPCELA_REC_PPRIO
           FROM  DB2PRD.V01EVNTO_CDULA
           WHERE    CJUNC_DEPDC = :WRK-B007-CJUNC-DEPDC
           AND      CCDULA      = :WRK-B007-CCDULA
           ORDER BY
             CTPO_EVNTO_RURAL
           , DEVNTO
           END-EXEC.


      *----------------------------------------------------------------*
      *    CSR04-RUECV011 - SELECAO DE CEDULAS                         *
      *----------------------------------------------------------------*

           EXEC SQL
             DECLARE CSR04-RUECV011 CURSOR FOR
           SELECT
             CJUNC_DEPDC
           , CCDULA
           , CSEQ_ITEM_RENDA
           , RITEM_RENDA
           , QITEM_RENDA
           , VITEM_RENDA
           , PRBATE_ITEM_RENDA
           FROM  DB2PRD.V01RENDA_CDULA
           WHERE    CJUNC_DEPDC = :WRK-B007-CJUNC-DEPDC
           AND      CCDULA      = :WRK-B007-CCDULA
           ORDER BY
             CSEQ_ITEM_RENDA
           END-EXEC.


      *----------------------------------------------------------------*
      *    CSR05-RUECV012 - SELECAO DE CEDULAS                         *
      *----------------------------------------------------------------*

           EXEC SQL
             DECLARE CSR05-RUECV012 CURSOR FOR
           SELECT
             CCONTR
           , CTPO_EVNTO_RURAL
           , DEVNTO
           , VEVNTO_RURAL
           , VPCELA_REC_PPRIO
           FROM  DB2PRD.V01EVNTO_RURAL
           WHERE   CCONTR = :WRK-B007-CCONTR
           ORDER BY
             CTPO_EVNTO_RURAL
           , DEVNTO
           END-EXEC.


      *----------------------------------------------------------------*
      *    CSR06-RUECB014 - SELECAO DE CEDULAS                         *
      *----------------------------------------------------------------*

           EXEC SQL
             DECLARE CSR06-RUECB014 CURSOR FOR
           SELECT
             CCONTR
           , CSEQ_PSSOA
           , CTPO_PSSOA
           , CTPO_RESP_PSSOA
           , IPSSOA
           , CCGC_CPF
           , CFLIAL_CGC
           , CCTRL_CPF_CGC
           , VRESP_RURAL
           , VRESP_GRAL
           , VSDO_MED_TMES
           , VSDO_MED_SEMTL
           , VRENDA_AGROP_OUTRO
           , VRENDA_NAO_AGROP
           , DCAD
           , DANO_RENDM
           , CSEQ_HIPOT_RURAL
           , CTPO_DOCTO_PSSOA
           , CDOCTO_PSSOA
           , CORG_EMISR_DOCTO
           , CUND_ORG_EMISR
           , CSEXO
           , CEST_CVIL_RGIME
           , DEST_CVIL
           , CNAC
           , CSGL_UF
           , INAT_ESTRG
           , ELOGDR
           , ENRO_LOGDR
           , ECOMPL_LOGDR
           , EBAIRO_LOGDR
           , CCEP
           , CCEP_COMPL
           , CMUN_RURAL
           , CCATEG_PROFS_CREDT
           , CPROFS_PSSOA
           , CCLASS_ATVDD_ECONC
           , CRAMO_ATVDD_ECONC
           , CSRAMO_ATVDD_ECONC
           , CATVDD_ECONC
           FROM  DB2PRD.PESSOA_CONTR_RURAL
           WHERE   CCONTR = :WRK-B007-CCONTR
           ORDER BY
             CSEQ_PSSOA
           END-EXEC.

      *----------------------------------------------------------------*
      *    CSR07-RUECB017 - SELECAO DE CEDULAS                         *
      *----------------------------------------------------------------*

           EXEC SQL
             DECLARE CSR07-RUECB017 CURSOR FOR
           SELECT
             CCONTR
           , CSEQ_HIPOT_RURAL
           , IPPRIE
           , RLOC_IMOV
           , MAREA_IMOV
           , CUND_MEDD
           , DAQUIS_IMOV
           , CFL_LIVRO_REG_IMOV
           , CLIVRO_REG_IMOV
           , ICARTR
           , ICOMAR
           , IDOCTO_POSSE_IMOV
           , VAVALC_IMOV
           , CMATR_IMOV_RURAL
           , CGRAU_PNHOR
           , CSEQ_ESCRT_ONUS
           , CSEQ_PATRM_IMOV
           , CMUN_RURAL
           , CCARTR_IMOV_REG
           , CGLEBA_IMOV_HIPOT
           , CLOTE_IMOV_HIPOT
           FROM  DB2PRD.HIPOT_CONTR_RURAL
           WHERE   CCONTR = :WRK-B007-CCONTR
           ORDER BY
             CSEQ_HIPOT_RURAL
           END-EXEC.


      *----------------------------------------------------------------*
      *    CSR08-RUECV018 - SELECAO DE CEDULAS                         *
      *----------------------------------------------------------------*

           EXEC SQL
             DECLARE CSR08-RUECV018 CURSOR FOR
           SELECT
             CCONTR
           , CRSUMO_MSGEM_INCON
           , RMSGEM_INCON
           , CINDCD_RESP_APROV
           FROM  DB2PRD.V01INCON_CONTR
           WHERE   CCONTR = :WRK-B007-CCONTR
           ORDER BY
             CRSUMO_MSGEM_INCON
           END-EXEC.


      *----------------------------------------------------------------*
      *    CSR09-RUECV019 - SELECAO DE CEDULAS                         *
      *----------------------------------------------------------------*

           EXEC SQL
             DECLARE CSR09-RUECV019 CURSOR FOR
           SELECT
             CJUNC_DEPDC
           , CCDULA
           , CRSUMO_MSGEM_INCON
           , RMSGEM_INCON
           , CINDCD_RESP_APROV
           FROM  DB2PRD.V01INCON_CDULA
           WHERE   CJUNC_DEPDC = :WRK-B007-CJUNC-DEPDC
           AND  CCDULA = :WRK-B007-CCDULA
           ORDER BY
             CRSUMO_MSGEM_INCON
           END-EXEC.


      *----------------------------------------------------------------*
      *    CSR10-RUECV022 - SELECAO DE CEDULAS                         *
      *----------------------------------------------------------------*

           EXEC SQL
             DECLARE CSR10-RUECV022 CURSOR FOR
           SELECT
             CCONTR
           , CPCELA_LIBRC
           , RTPO_OPER_ORCAM
           , CTPO_LIBRC_ORCAM
           , DLIBRC_OPER_ORCAM
           , VFINCD_OPER_ORCAM
           , VPCELA_REC_PPRIO
           , CINDCD_LIBRC_PCELA
           , DFIM_OPER_ORCAM
           FROM  DB2PRD.V01ORCAM_RURAL
           WHERE   CCONTR = :WRK-B007-CCONTR
           ORDER BY
             CPCELA_LIBRC
           END-EXEC.


      *----------------------------------------------------------------*
      *    CSR11-RUECV023 - SELECAO DE CEDULAS                         *
      *----------------------------------------------------------------*

           EXEC SQL
             DECLARE CSR11-RUECV023 CURSOR FOR
           SELECT
             CCONTR
           , CPCELA_LIBRC
           , CISUMO_ORCAM_RURAL
           , RISUMO_ORCAM_RURAL
           , RQTD_UND_ISUMO
           , VISUMO_ORCAM_RURAL
           FROM  DB2PRD.V01ISUMO_ORCAM
           WHERE   CCONTR = :WRK-B007-CCONTR
           ORDER BY
             CPCELA_LIBRC
           END-EXEC.


      *----------------------------------------------------------------*
      *    CSR12-RUECB024 - SELECAO DE CEDULAS                         *
      *----------------------------------------------------------------*

           EXEC SQL
             DECLARE CSR12-RUECB024 CURSOR FOR
           SELECT
             CCONTR
           , CLIN_OBS_ORCAM
           , RLIN_OBS_ORCAM
           FROM  DB2PRD.OBS_ORCAM_RURAL
           WHERE   CCONTR = :WRK-B007-CCONTR
           ORDER BY
             CLIN_OBS_ORCAM
           END-EXEC.


      *----------------------------------------------------------------*
      *    CSR13-RUECV025 - SELECAO DE CEDULAS                         *
      *----------------------------------------------------------------*

           EXEC SQL
             DECLARE CSR13-RUECV025 CURSOR FOR
           SELECT
             CJUNC_DEPDC
           , CCDULA
           , CPCELA_LIBRC
           , RTPO_OPER_ORCAM
           , CTPO_LIBRC_ORCAM
           , DLIBRC_OPER_ORCAM
           , VFINCD_OPER_ORCAM
           , VPCELA_REC_PPRIO
           , CTPO_INCON_ORCAM
           , CINDCD_LIBRC_PCELA
           FROM  DB2PRD.V01ORCAM_CDULA
           WHERE   CJUNC_DEPDC = :WRK-B007-CJUNC-DEPDC
           AND     CCDULA      = :WRK-B007-CCDULA
           ORDER BY
             CPCELA_LIBRC
           END-EXEC.


      *----------------------------------------------------------------*
      *    CSR14-RUECV026 - SELECAO DE CEDULAS                         *
      *----------------------------------------------------------------*

           EXEC SQL
             DECLARE CSR14-RUECV026 CURSOR FOR
           SELECT
             CJUNC_DEPDC
           , CCDULA
           , CPCELA_LIBRC
           , CISUMO_ORCAM_RURAL
           , RISUMO_ORCAM_RURAL
           , RQTD_UND_ISUMO
           , VISUMO_ORCAM_RURAL
           FROM  DB2PRD.V01ISUMO_CDULA
           WHERE   CJUNC_DEPDC = :WRK-B007-CJUNC-DEPDC
           AND  CCDULA         = :WRK-B007-CCDULA
           ORDER BY
             CPCELA_LIBRC
           END-EXEC.


      *----------------------------------------------------------------*
      *    CSR15-RUECB027 - SELECAO DE CEDULAS                         *
      *----------------------------------------------------------------*

           EXEC SQL
             DECLARE CSR15-RUECB027 CURSOR FOR
           SELECT
             CJUNC_DEPDC
           , CCDULA
           , CLIN_OBS_ORCAM
           , RLIN_OBS_ORCAM
           FROM  DB2PRD.OBS_ORCAM_CDULA
           WHERE   CJUNC_DEPDC = :WRK-B007-CJUNC-DEPDC
           AND     CCDULA      = :WRK-B007-CCDULA
           ORDER BY
             CLIN_OBS_ORCAM
           END-EXEC.


      *----------------------------------------------------------------*
      *    CSR16-RUECB028 - SELECAO DE CEDULAS                         *
      *----------------------------------------------------------------*

           EXEC SQL
             DECLARE CSR16-RUECB028 CURSOR FOR
           SELECT
             CCONTR
           , CSEQ_ARZEM_RURAL
           , IARZEM_PRODT_RURAL
           , IPPRIE_ARZEM_PRODT
           , RLOC_ARZEM_PRODT
           , CREG_CONAB_ARZEM
           , CREG_CIBRAZEM
           , CCNPJ_EMPR
           , CFLIAL_CNPJ_EMPR
           , CCTRL_CPF_CNPJ
           , QEXPDI_ARZEM_ENSAC
           , QEXPDI_ARZEM_GRANL
           , CMUN_RURAL
           FROM  DB2PRD.ARZEM_PRODT_RURAL
           WHERE   CCONTR = :WRK-B007-CCONTR
           ORDER BY
             CSEQ_ARZEM_RURAL
           END-EXEC.



      *----------------------------------------------------------------*
      *    CSR18-RUECB034 - SELECAO DE CEDULAS                         *
      *----------------------------------------------------------------*

           EXEC SQL
             DECLARE CSR18-RUECB034 CURSOR FOR
           SELECT
             CJUNC_DEPDC
           , CCDULA
           , CLIN_FIS_TEXTO
           , RLIN_MAX
           , CPAG
           FROM  DB2PRD.HIST_IMPRE_CDULA
           WHERE   CJUNC_DEPDC = :WRK-B007-CJUNC-DEPDC
           AND     CCDULA      = :WRK-B007-CCDULA
           ORDER BY
             CLIN_FIS_TEXTO
           END-EXEC.



      *----------------------------------------------------------------*
      *    CSR19-RUECB035 - SELECAO DE CEDULAS                         *
      *----------------------------------------------------------------*

           EXEC SQL
             DECLARE CSR19-RUECB035 CURSOR FOR
           SELECT
             CJUNC_DEPDC
           , CCDULA
           , CSEQ_HIPOT_RURAL
           , IPPRIE_IMOV
           , RLOC_IMOV
           , MAREA_IMOV
           , CUND_MEDD
           , DAQUIS_IMOV
           , CFL_LIVRO_REG_IMOV
           , CLIVRO_REG_IMOV
           , ICARTR
           , ICOMAR
           , IDOCTO_POSSE_IMOV
           , VAVALC_IMOV
           , CMATR_IMOV_RURAL
           , CGRAU_PNHOR
           , CSEQ_ESCRT_ONUS
           , CSEQ_PATRM_IMOV
           , CMUN_RURAL
           , CCARTR_IMOV_REG
           , CGLEBA_IMOV_HIPOT
           , CLOTE_IMOV_HIPOT
           FROM  DB2PRD.HIPOTECA_TEMPR
           WHERE   CJUNC_DEPDC = :WRK-B007-CJUNC-DEPDC
           AND     CCDULA      = :WRK-B007-CCDULA
           ORDER BY
               CSEQ_HIPOT_RURAL
           END-EXEC.


      *----------------------------------------------------------------*
      *    CSR20-RUECB036 - SELECAO DE CEDULAS                         *
      *----------------------------------------------------------------*

           EXEC SQL
             DECLARE CSR20-RUECB036 CURSOR FOR
           SELECT
             CJUNC_DEPDC
           , CCDULA
           , CSEQ_HIPOT_RURAL
           , CLIN_FIS_TEXTO
           , RLIN_MAX
           FROM  DB2PRD.CONFR_HIPOT_TEMPR
           WHERE   CJUNC_DEPDC = :WRK-B007-CJUNC-DEPDC
           AND     CCDULA      = :WRK-B007-CCDULA
           ORDER BY
             CSEQ_HIPOT_RURAL
           END-EXEC.


      *----------------------------------------------------------------*
      *    CSR21-RUECB038 - SELECAO DE CEDULAS                         *
      *----------------------------------------------------------------*

           EXEC SQL
             DECLARE CSR21-RUECB038 CURSOR FOR
           SELECT
             CCONTR
           , CSEQ_HIPOT_RURAL
           , CLIN_FIS_TEXTO
           , RLIN_MAX
           FROM  DB2PRD.CONFR_HIPOT_RURAL
           WHERE   CCONTR = :WRK-B007-CCONTR
           ORDER BY
             CSEQ_HIPOT_RURAL
           END-EXEC.


      *----------------------------------------------------------------*
      *    CSR22-RUECB040 - SELECAO DE CEDULAS                         *
      *----------------------------------------------------------------*

           EXEC SQL
             DECLARE CSR22-RUECB040 CURSOR FOR
           SELECT
             CCONTR
           , CSEQ_MATR_RURAL
           , CINDCD_AREA_PRINC
           , CMATR_IMOV_RURAL
           , IIMOV_RURAL
           , MAREA_IMOV_RURAL
           , CMUN_RURAL
           , CSEQ_IMOV_RURAL
           , CID_GLEBA
           , MLATTD_PTO_CNTRL
           , MLOGTD_PTO_CNTRL
           FROM  DB2PRD.MATR_CONTR_RURAL
           WHERE   CCONTR = :WRK-B007-CCONTR
           ORDER BY
             CSEQ_MATR_RURAL
           END-EXEC.


      *----------------------------------------------------------------*
      *    CSR23-RUECB041 - SELECAO DE CEDULAS                         *
      *----------------------------------------------------------------*

           EXEC SQL
             DECLARE CSR23-RUECB041 CURSOR FOR
           SELECT
             CJUNC_DEPDC
           , CCDULA
           , CSEQ_MATR_RURAL
           , CINDCD_AREA_PRINC
           , CMATR_IMOV_RURAL
           , IIMOV_RURAL
           , MAREA_IMOV_RURAL
           , CSEQ_IMOV_RURAL
           , CMUN_RURAL
           , CID_GLEBA
           , MLATTD_PTO_CNTRL
           , MLOGTD_PTO_CNTRL
           FROM  DB2PRD.MATR_CDULA_RURAL
           WHERE   CJUNC_DEPDC = :WRK-B007-CJUNC-DEPDC
           AND     CCDULA      = :WRK-B007-CCDULA
           ORDER BY
               CSEQ_MATR_RURAL
           END-EXEC.


      *----------------------------------------------------------------*
      *    CSR25-RUECB052 - SELECAO DE CEDULAS                         *
      *----------------------------------------------------------------*

           EXEC SQL
             DECLARE CSR25-RUECB052 CURSOR FOR
           SELECT
             CCONTR
           , CSEQ_HIST_IMOV
           , RIMOV_RURAL
           , RTPO_EXPLO_IMOV
           , PIMOV_CLI
           , ELOGDR
           , ENRO_LOGDR
           , ECOMPL_LOGDR
           , EBAIRO_LOGDR
           , CMUN_RURAL
           , CCEP
           , CCEP_COMPL
           , MAREA_CULTR_AGRIC
           , MAREA_PASTA
           , MDMAIS_AREA
           , RROTRO_ACSSO
           , HULT_ATULZ_EFETU
           , CFUNC_BDSCO
           , RTPO_IMOV_ARREN
           , DAQUIS_IMOV
           , RTPO_DOCTO
           , CLIVRO_REG_IMOV
           , DREG_IMOV
           , CINDCD_IMOV_IMPNH
           , VATUAL_IMOV_RURAL
           , CCARTR_IMOV_REG
           , CMATR_IMOV_REG
           , NFL_IMOV_REG
           , IRGIAO_JUDIC_IMOV
           , ICARTR_CONTR_RURAL
           , CGLEBA_IMOV_HIPOT
           , CLOTE_IMOV_HIPOT
           FROM  DB2PRD.THIST_IMOV_RURAL
           WHERE   CCONTR = :WRK-B007-CCONTR
           ORDER BY
               CSEQ_HIST_IMOV
           END-EXEC.


      *----------------------------------------------------------------*
      *    CSR26-RUECB053 - SELECAO DE CEDULAS                         *
      *----------------------------------------------------------------*

           EXEC SQL
             DECLARE CSR26-RUECB053 CURSOR FOR
           SELECT
             CCONTR
           , CPRODT_RURAL
           , CSEQ_HIST_RBNHO
           , QRBNHO_IMOV
           , RESPEC_RBNHO
           , HULT_ATULZ_EFETU
           , CFUNC_BDSCO
           FROM  DB2PRD.THIST_RBNHO_CONTR
           WHERE   CCONTR = :WRK-B007-CCONTR
           ORDER BY
             CPRODT_RURAL
           , CSEQ_HIST_RBNHO
           END-EXEC.


      *----------------------------------------------------------------*
      *    CSR27-RUECB054 - SELECAO DE CEDULAS                         *
      *----------------------------------------------------------------*

           EXEC SQL
             DECLARE CSR27-RUECB054 CURSOR FOR
           SELECT
             CCONTR
           , CPRODT_RURAL
           , URENDA_AGROP
           , QRENDA_AGROP
           , VRENDA_AGROP
           , PRBATE_RENDA_CLI
           , HULT_ATULZ_EFETU
           , CFUNC_BDSCO
           FROM  DB2PRD.THIST_RENDA_AGROP
           WHERE   CCONTR = :WRK-B007-CCONTR
           ORDER BY
             CPRODT_RURAL
           END-EXEC.

      *----------------------------------------------------------------*
      *    CSR28-RUECB064 - SELECAO DE CEDULAS                         *
      *----------------------------------------------------------------*

           EXEC SQL
             DECLARE CSR28-RUECB064 CURSOR FOR
           SELECT
             CCONTR
           , CSEQ_HIST_DEVLC
           , DDEVLC_CONTR
           , VFINAN
           , CFUNC_BDSCO
           , RMOTVO_DEVLC_CONTR
           FROM  DB2PRD.THIST_DEVLC_CONTR
           WHERE   CCONTR = :WRK-B007-CCONTR
           ORDER BY
             CSEQ_HIST_DEVLC
           END-EXEC.


      *----------------------------------------------------------------*
      *    CSR29-RUECB065 - SELECAO DE CEDULAS                         *
      *----------------------------------------------------------------*

           EXEC SQL
             DECLARE CSR29-RUECB065 CURSOR FOR
           SELECT
             CCONTR_CREDT_RURAL
           , NSEQ_REG_CARTR
           , CCARTR_CONTR_RURAL
           , ICARTR_CONTR_RURAL
           , CMATR_CARTR_RURAL
           , CMUN_CARTR
           , CREG_PNHOR_RURAL
           , CLIVRO_PNHOR_RURAL
           , DREG_PNHOR_RURAL
           , CREG_HIPOT_RURAL
           , CLIVRO_HIPOT_RURAL
           , DREG_HIPOT_RURAL
           , CSIT_PNHOR_HIPOT
           , CORIGE_CDULA
           , DINCL_CARTR_RURAL
           , CFUNC_BDSCO_INCL
           , HULT_ATULZ
           , CFUNC_ULT_ATULZ
           FROM  DB2PRD.TCARTR_CONTR_RURAL
           WHERE   CCONTR_CREDT_RURAL = :WRK-B007-CCONTR
           ORDER BY
             NSEQ_REG_CARTR
           END-EXEC.


      *----------------------------------------------------------------*
      *    CSR30-RUECB066 - SELECAO DE CEDULAS                         *
      *----------------------------------------------------------------*

           EXEC SQL
             DECLARE CSR30-RUECB066 CURSOR FOR
           SELECT
             CAG_BCRIA
           , CCDULA_CREDT_RURAL
           , NSEQ_OBS_LICEN
           , ROBS_LICEN_AMBTL
           , DINCL_OBS_LICEN
           , CFUNC_BDSCO_INCL
           , HULT_ATULZ
           , CFUNC_ULT_ATULZ
           FROM  DB2PRD.TOBS_LICEN_AMBTL
           WHERE         CAG_BCRIA = :WRK-B007-CJUNC-DEPDC
           AND  CCDULA_CREDT_RURAL = :WRK-B007-CCDULA
           ORDER BY
             NSEQ_OBS_LICEN
           END-EXEC.


      *----------------------------------------------------------------*
      *    CSR31-RUECB073 - SELECAO DE CEDULAS                         *
      *----------------------------------------------------------------*

           EXEC SQL
             DECLARE CSR31-RUECB073 CURSOR FOR
           SELECT
             CCONTR_CREDT_RURAL
           , DVCTO_PCELA_CONTR
           , NLIM_OPER
           , DEVNTO_LIM_OPER
           , RMANUT_CONTR
           , VUTLZD_LIM_OPER
           , DULT_ATULZ
           , CUSUAR_SENHA
           FROM  DB2PRD.TUTILZ_LIM_OPER
           WHERE   CCONTR_CREDT_RURAL = :WRK-B007-CCONTR
           ORDER BY
             DVCTO_PCELA_CONTR
           , NLIM_OPER
           END-EXEC.


      *----------------------------------------------------------------*
      *    CSR32-RUECB074 - SELECAO DE CEDULAS                         *
      *----------------------------------------------------------------*

           EXEC SQL
             DECLARE CSR32-RUECB074 CURSOR FOR
           SELECT
             CJUNC_DEPDC
           , CCDULA_CREDT_RURAL
           , CSEQ_PSSOA
           , ICONJG_PSSOA
           , CCPF_CONJG_PSSOA
           , CCTRL_CONJG_PSSOA
           , CTPO_DOCTO_PSSOA
           , CDOCTO_PSSOA
           , CORG_EMISR_DOCTO
           , CUND_ORG_EMISR
           , CNAC
           , CSGL_UF
           , INAT_ESTRG
           , CCATEG_PROFS_CREDT
           , CPROFS_PSSOA
           FROM  DB2PRD.TINFO_CONJG_CDULA
           WHERE         CJUNC_DEPDC = :WRK-B007-CJUNC-DEPDC
             AND  CCDULA_CREDT_RURAL = :WRK-B007-CCDULA
           ORDER BY
             CSEQ_PSSOA
           END-EXEC.


      *----------------------------------------------------------------*
      *    CSR33-RUECB075 - SELECAO DE CEDULAS                         *
      *----------------------------------------------------------------*

           EXEC SQL
             DECLARE CSR33-RUECB075 CURSOR FOR
           SELECT
             CCONTR_CREDT_RURAL
           , CSEQ_PSSOA
           , ICONJG_PSSOA
           , CCPF_CONJG_PSSOA
           , CCTRL_CONJG_PSSOA
           , CTPO_DOCTO_PSSOA
           , CDOCTO_PSSOA
           , CORG_EMISR_DOCTO
           , CUND_ORG_EMISR
           , CNAC
           , CSGL_UF
           , INAT_ESTRG
           , CCATEG_PROFS_CREDT
           , CPROFS_PSSOA
           FROM  DB2PRD.TINFO_CONJG_CONTR
           WHERE   CCONTR_CREDT_RURAL = :WRK-B007-CCONTR
           ORDER BY
             CSEQ_PSSOA
           END-EXEC.


      *----------------------------------------------------------------*
      *    CSR34-RUECB096 - SELECAO DE CEDULAS                         *
      *----------------------------------------------------------------*

           EXEC SQL
             DECLARE CSR34-RUECB096 CURSOR FOR
           SELECT
             CCONTR_CREDT_RURAL
           , NSEQ_HIST_SIT
           , CINDCD_SIT_CDULA
           , CREFT_BACEN
           , DULT_ATULZ
           , CFUNC_ULT_ATULZ
           , CREFT_RECOR
           , CMOTVO_EXCL_RECOR
           FROM  DB2PRD.THIST_SIT_CDULA
           WHERE   CCONTR_CREDT_RURAL = :WRK-B007-CCONTR
           ORDER BY
             NSEQ_HIST_SIT
           END-EXEC.

AC0613*----------------------------------------------------------------*
AC0613*    CSR35-RUECB097 - SELECAO DE CEDULAS                         *
AC0613*----------------------------------------------------------------*
AC0613
AC0613     EXEC SQL
AC0613     DECLARE CSR35-RUECB097 CURSOR FOR
AC0613     SELECT CJUNC_DEPDC,
AC0613            CCDULA_CREDT_RURAL,
AC0613            NITEM_COMCZ,
AC0613            CPROTR_RURAL,
AC0613            CNOTA_FSCAL_VDA,
AC0613            DNOTA_FSCAL_VDA,
AC0613            QUND_NOTA_FSCAL,
AC0613            VUNTAR_NOTA_FSCAL,
AC0613            CUND_MEDD,
AC0613            CMUN_RURAL,
AC0613            DINIC_SAFRA,
AC0613            DFIM_SAFRA,
AC0613            QUND_FINCD_RURAL,
AC0613            VPRECO_MIN_RURAL,
AC0613            VUTLZD_NOTA_FSCAL,
AC0613            CSIT_VNCLO,
AC0613            HINCL,
AC0613            HULT_ATULZ,
AC0613            CUSUAR_RESP,
AC0613            CMODLD_RURAL,
AC0613            CPRODT_RURAL,
AC0613            CESTCA_SAFRA
AC0613     FROM  DB2PRD.TCDULA_COMCZ
AC0613     WHERE CJUNC_DEPDC        = :WRK-B007-CJUNC-DEPDC
AC0613     AND   CCDULA_CREDT_RURAL = :WRK-B007-CCDULA
AC0613     ORDER BY CJUNC_DEPDC,
AC0613              CCDULA_CREDT_RURAL,
AC0613              NITEM_COMCZ
AC0613     END-EXEC.

AC0613*----------------------------------------------------------------*
AC0613*    CSR36-RUECB099 - SELECAO DE CEDULAS                         *
AC0613*----------------------------------------------------------------*
AC0613
AC0613     EXEC SQL
AC0613     DECLARE CSR36-RUECB099 CURSOR FOR
AC0613     SELECT CCONTR_CREDT_RURAL,
AC0613            NENVIO_RECOR,
AC0613            CCNPJ_ENTID_RESP,
AC0613            CFLIAL_CNPJ_ENTID,
AC0613            CCTRL_CNPJ_ENTID,
AC0613            CCNPJ_PARTC,
AC0613            CFLIAL_CNPJ_PARTC,
AC0613            CCTRL_CNPJ_PARTC ,
AC0613            CSIT_OPER_RURAL,
AC0613            DCDULA,
AC0613            DVCTO_CONTR,
AC0613            CCDULA_RECOR,
AC0613            CTPO_INSTT_CREDT,
AC0613            VFINAN,
AC0613            CCATEG_PROTR_RECOR,
AC0613            CPROG_RURAL,
AC0613            CTPO_FONTE_RECOR,
AC0613            CMUN_REFT_BACEN,
AC0613            CEMPTO_BACEN,
AC0613            CSIST_PROD,
AC0613            VPCELA_LIBRC,
AC0613            VPCELA_REC_PPRIO,
AC0613            PALIQT_PROAGRO,
AC0613            PJURO_ENCAR_FINCR,
AC0613            CTPO_ENCAR_COMPL,
AC0613            CTSORO_NACIO,
AC0613            PRISCO_TSORO_NACIO,
AC0613            PRISCO_FUNDO_CNSTT,
AC0613            QAREA_RECOR,
AC0613            QUND_FINCD_RURAL,
AC0613            CUND_MEDD_FINCD,
AC0613            QUND_PROD_PROVV,
AC0613            CUND_MEDD_PROVV,
AC0613            DINIC_SAFRA,
AC0613            DFIM_SAFRA,
AC0613            CTPO_GARNT_RECOR,
AC0613            VRECTA_BRUTA_RECOR,
AC0613            DMOVTO_RECOR
AC0613     FROM  DB2PRD.TCONTR_ENVIO_RECOR
AC0613     WHERE CCONTR_CREDT_RURAL = :WRK-B007-CCONTR
AC0613     ORDER BY CCONTR_CREDT_RURAL,
AC0613              NENVIO_RECOR
AC0613     END-EXEC.

AC0613*----------------------------------------------------------------*
AC0613*    CSR37-RUECB09A - SELECAO DE CEDULAS                         *
AC0613*----------------------------------------------------------------*
AC0613
AC0613     EXEC SQL
AC0613     DECLARE CSR37-RUECB09A CURSOR FOR
AC0613     SELECT CCONTR_CREDT_RURAL,
AC0613            NENVIO_RECOR,
AC0613            NREG_MSGEM_RECOR,
AC0613            CSIT_MSGEM,
AC0613            HINCL,
AC0613            WCONTD_MSGEM_RURAL
AC0613     FROM  DB2PRD.TENVIO_RECOR_TEMPR
AC0613     WHERE CCONTR_CREDT_RURAL = :WRK-B007-CCONTR
AC0613     ORDER BY CCONTR_CREDT_RURAL,
AC0613              NENVIO_RECOR
AC0613     END-EXEC.

AC0613*----------------------------------------------------------------*
AC0613*    CSR38-RUECB09B - SELECAO DE CEDULAS                         *
AC0613*----------------------------------------------------------------*
AC0613
AC0613     EXEC SQL
AC0613     DECLARE CSR38-RUECB09B CURSOR FOR
AC0613     SELECT CCONTR_CREDT_RURAL,
AC0613            NENVIO_RECOR,
AC0613            HULT_ATULZ,
AC0613            CMSGEM_RECOR,
AC0613            HINCL,
AC0613            CCTRL_RQUIS_FINCR,
AC0613            CUNIC_MSGEM,
AC0613            CREFT_RECOR,
AC0613            CSIT_MSGEM,
AC0613            CUSUAR_RESP
AC0613     FROM  DB2PRD.THIST_ENVIO_RECOR
AC0613     WHERE CCONTR_CREDT_RURAL = :WRK-B007-CCONTR
AC0613     ORDER BY CCONTR_CREDT_RURAL,
AC0613              NENVIO_RECOR
AC0613     END-EXEC.

AC0613*----------------------------------------------------------------*
AC0613*    CSR39-RUECB09D - SELECAO DE CEDULAS                         *
AC0613*----------------------------------------------------------------*
AC0613
AC0613     EXEC SQL
AC0613     DECLARE CSR39-RUECB09D CURSOR FOR
AC0613     SELECT CCONTR_CREDT_RURAL,
AC0613            NENVIO_RECOR,
AC0613            NPCELA_CONTR,
AC0613            DVCTO_PCELA_CONTR,
AC0613            VPCELA_RURAL
AC0613     FROM  DB2PRD.TPCELA_ENVIO_RECOR
AC0613     WHERE CCONTR_CREDT_RURAL = :WRK-B007-CCONTR
AC0613     ORDER BY CCONTR_CREDT_RURAL,
AC0613              NENVIO_RECOR
AC0613     END-EXEC.

AC0613*----------------------------------------------------------------*
AC0613*    CSR40-RUECB09E - SELECAO DE CEDULAS                         *
AC0613*----------------------------------------------------------------*
AC0613
AC0613     EXEC SQL
AC0613     DECLARE CSR40-RUECB09E CURSOR FOR
AC0613     SELECT CCONTR_CREDT_RURAL,
AC0613            NENVIO_RECOR,
AC0613            NPSSOA_CONTR,
AC0613            CTPO_RESP_PSSOA,
AC0613            CTPO_PSSOA,
AC0613            CCNPJ_CPF,
AC0613            CFLIAL_CNPJ,
AC0613            CCTRL_CNPJ_CPF
AC0613     FROM  DB2PRD.TPSSOA_ENVIO_RECOR
AC0613     WHERE CCONTR_CREDT_RURAL = :WRK-B007-CCONTR
AC0613     ORDER BY CCONTR_CREDT_RURAL,
AC0613              NENVIO_RECOR
AC0613     END-EXEC.

AC0613*----------------------------------------------------------------*
AC0613*    CSR41-RUECB09F - SELECAO DE CEDULAS                         *
AC0613*----------------------------------------------------------------*
AC0613
AC0613     EXEC SQL
AC0613     DECLARE CSR41-RUECB09F CURSOR FOR
AC0613     SELECT CCONTR_CREDT_RURAL,
AC0613            NENVIO_RECOR,
AC0613            NPRODT_RURAL_RECOR,
AC0613            CPRODT_RURAL
AC0613     FROM  DB2PRD.TPRODT_RURAL_RECOR
AC0613     WHERE CCONTR_CREDT_RURAL = :WRK-B007-CCONTR
AC0613     ORDER BY CCONTR_CREDT_RURAL,
AC0613              NENVIO_RECOR
AC0613     END-EXEC.

AC0613*----------------------------------------------------------------*
AC0613*    CSR42-RUECB09G - SELECAO DE CEDULAS                         *
AC0613*----------------------------------------------------------------*
AC0613
AC0613     EXEC SQL
AC0613     DECLARE CSR42-RUECB09G CURSOR FOR
AC0613     SELECT CCONTR_CREDT_RURAL,
AC0613            NPRODT_RURAL_CSCDO,
AC0613            CPRODT_RURAL,
AC0613            DULT_ATULZ,
AC0613            CUSUAR_RESP
AC0613     FROM  DB2PRD.TPRODT_RURAL_CSCDO
AC0613     WHERE CCONTR_CREDT_RURAL = :WRK-B007-CCONTR
AC0613     ORDER BY CCONTR_CREDT_RURAL,
AC0613              NPRODT_RURAL_CSCDO
AC0613     END-EXEC.

AC0613*----------------------------------------------------------------*
AC0613*    CSR43-RUECB09H - SELECAO DE CEDULAS                         *
AC0613*----------------------------------------------------------------*
AC0613
AC0613     EXEC SQL
AC0613     DECLARE CSR43-RUECB09H CURSOR FOR
AC0613     SELECT CDEPDC,
AC0613            CCDULA_CREDT_RURAL,
AC0613            NPRODT_RURAL_TEMPR,
AC0613            CPRODT_RURAL,
AC0613            DULT_ATULZ,
AC0613            CUSUAR_RESP
AC0613     FROM  DB2PRD.TPRODT_RURAL_TEMPR
AC0613     WHERE CDEPDC             = :WRK-B007-CJUNC-DEPDC
AC0613     AND   CCDULA_CREDT_RURAL = :WRK-B007-CCDULA
AC0613     ORDER BY CDEPDC,
AC0613              CCDULA_CREDT_RURAL,
AC0613              NPRODT_RURAL_TEMPR
AC0613     END-EXEC.

AC0613*----------------------------------------------------------------*
AC0613*    CSR44-RUECB09I - SELECAO DE CEDULAS                         *
AC0613*----------------------------------------------------------------*
AC0613
AC0613     EXEC SQL
AC0613     DECLARE CSR44-RUECB09I CURSOR FOR
AC0613     SELECT CCONTR_CREDT_RURAL,
AC0613            DVCTO_PCELA_CONTR,
AC0613            DSELEC_LASTR,
AC0613            NPCELA_PGTO_RURAL,
AC0613            CMODLD_JURO,
AC0613            VRJURO,
AC0613            VPCELA_ATULZ,
AC0613            VRESGT_LASTR,
AC0613            VSDO_ATULZ,
AC0613            CSELEC_LASTR,
AC0613            DREG_LASTR,
AC0613            CREG_LASTR,
AC0613            CCMARA_REG_LASTR,
AC0613            NSERIE_REG_LASTR,
AC0613            CCONTR_CMARA,
AC0613            CSIT_LASTR,
AC0613            CJUNC_DEPDC,
AC0613            CSECAO,
AC0613            CUSUAR_RESP_INCL,
AC0613            RMOTVO_EXCL_LASTR,
AC0613            CUSUAR_RESP_EXCL,
AC0613            DEXCL_LASTR
AC0613     FROM  DB2PRD.TLASTR_CONTR_RURAL
AC0613     WHERE CCONTR_CREDT_RURAL = :WRK-B007-CCONTR
AC0613     ORDER BY CCONTR_CREDT_RURAL,
AC0613              DVCTO_PCELA_CONTR
AC0613     END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)  VALUE
           '*** FIM DA WORKING RUEC1450  ***'.
      *----------------------------------------------------------------*

      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       0000-INICIO                     SECTION.
      *----------------------------------------------------------------*

           CALL 'BRAD1050'.

           PERFORM 1000-INICIALIZAR.

           PERFORM 2000-PROCESSA
             UNTIL WRK-FIM-SELCDULA    EQUAL 'S'.

           PERFORM 9000-FINALIZAR.

           STOP RUN.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*

           OPEN INPUT  ARQDATAS
                       SELCDULA
                OUTPUT RUEC008S
                       RUEC009S
                       RUEC010S
                       RUEC011S
                       RUEC012S
                       RUEC014S
                       RUEC017S
                       RUEC018S
                       RUEC019S
                       RUEC022S
                       RUEC023S
                       RUEC024S
                       RUEC025S
                       RUEC026S
                       RUEC027S
                       RUEC028S
                       RUEC033S
                       RUEC034S
                       RUEC035S
                       RUEC036S
                       RUEC038S
                       RUEC040S
                       RUEC041S
                       RUEC044S
                       RUEC052S
                       RUEC053S
                       RUEC054S
                       RUEC064S
                       RUEC065S
                       RUEC066S
                       RUEC073S
                       RUEC074S
                       RUEC075S
                       RUEC096S
AC0613                 RUEC097S
AC0613                 RUEC099S
AC0613                 RUEC09AS
AC0613                 RUEC09BS
AC0613                 RUEC09DS
AC0613                 RUEC09ES
AC0613                 RUEC09FS
AC0613                 RUEC09GS
AC0613                 RUEC09HS
AC0613                 RUEC09IS.

           MOVE WRK-ABERTURA           TO  WRK-OPERACAO.
           PERFORM 8000-TESTAR-FILE-STATUS.

           CALL  'BRAD7600'            USING WRK-DATA-HORA.

           PERFORM 1300-LER-ARQDATAS.

           IF WRK-FIM-ARQDATAS         EQUAL 'S'
              MOVE 'ARQDATAS'          TO  WRK-ARQUIVO-VAZIO
              MOVE 'APL'               TO  ERR-TIPO-ACESSO
              MOVE WRK-MSG-ARQ-VAZIO   TO  ERR-TEXTO
              PERFORM 9999-ROTINA-ERRO
           END-IF.

           PERFORM 1400-LER-SELCDULA.

           IF WRK-FIM-SELCDULA         EQUAL 'S'
              MOVE 'SELCDULA'          TO  WRK-ARQUIVO-VAZIO
              MOVE 'APL'               TO  ERR-TIPO-ACESSO
              MOVE WRK-MSG-ARQ-VAZIO   TO  ERR-TEXTO
              PERFORM 9999-ROTINA-ERRO
           END-IF.

           MOVE WRK-B007-CJUNC-DEPDC   TO  WRK-CJUNC-DEPDC-ANT.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1300-LER-ARQDATAS               SECTION.
      *----------------------------------------------------------------*

           READ  ARQDATAS              INTO   N090RURA.

           IF  WRK-FS-ARQDATAS         EQUAL '10'
               MOVE   'S'              TO WRK-FIM-ARQDATAS
               GO                      TO 1300-99-FIM
           END-IF.

           ADD   1                     TO ACU-LIDOS-ARQDATAS
           MOVE  WRK-LEITURA           TO WRK-OPERACAO
           PERFORM 8010-TESTAR-FS-ARQDATAS.

      *----------------------------------------------------------------*
       1300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1400-LER-SELCDULA               SECTION.
      *----------------------------------------------------------------*

           READ  SELCDULA              INTO   WRK-RUECB007.

           IF  WRK-FS-SELCDULA         EQUAL '10'
               MOVE   'S'              TO WRK-FIM-SELCDULA
               GO                      TO 1400-99-FIM
           END-IF.

           ADD   1                     TO ACU-LIDOS-SELCDULA
           MOVE  WRK-LEITURA           TO WRK-OPERACAO
           PERFORM 8020-TESTAR-FS-SELCDULA.

      *----------------------------------------------------------------*
       1400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2000-PROCESSA                   SECTION.
      *----------------------------------------------------------------*

           IF  WRK-B007-CJUNC-DEPDC  NOT EQUAL  WRK-CJUNC-DEPDC-ANT
               PERFORM 2050-TOTAL-AGENCIA-LIDA
               MOVE WRK-B007-CJUNC-DEPDC    TO  WRK-CJUNC-DEPDC-ANT
           END-IF.

           ADD 1                            TO  ACU-LIDOS-AGENCIA.

           MOVE  SPACES                     TO  WRK-FIM-RUEC008S
                                                WRK-FIM-RUEC009S
                                                WRK-FIM-RUEC010S
                                                WRK-FIM-RUEC011S
                                                WRK-FIM-RUEC012S
                                                WRK-FIM-RUEC014S
                                                WRK-FIM-RUEC017S
                                                WRK-FIM-RUEC018S
                                                WRK-FIM-RUEC019S
                                                WRK-FIM-RUEC022S
                                                WRK-FIM-RUEC023S
                                                WRK-FIM-RUEC024S
                                                WRK-FIM-RUEC025S
                                                WRK-FIM-RUEC026S
                                                WRK-FIM-RUEC027S
                                                WRK-FIM-RUEC028S
                                                WRK-FIM-RUEC033S
                                                WRK-FIM-RUEC034S
                                                WRK-FIM-RUEC035S
                                                WRK-FIM-RUEC036S
                                                WRK-FIM-RUEC038S
                                                WRK-FIM-RUEC040S
                                                WRK-FIM-RUEC041S
                                                WRK-FIM-RUEC044S
                                                WRK-FIM-RUEC052S
                                                WRK-FIM-RUEC053S
                                                WRK-FIM-RUEC054S
                                                WRK-FIM-RUEC064S
                                                WRK-FIM-RUEC065S
                                                WRK-FIM-RUEC066S
                                                WRK-FIM-RUEC073S
                                                WRK-FIM-RUEC074S
                                                WRK-FIM-RUEC075S
                                                WRK-FIM-RUEC096S
AC0613                                          WRK-FIM-RUEC097S
AC0613                                          WRK-FIM-RUEC099S
AC0613                                          WRK-FIM-RUEC09AS
AC0613                                          WRK-FIM-RUEC09BS
AC0613                                          WRK-FIM-RUEC09DS
AC0613                                          WRK-FIM-RUEC09ES
AC0613                                          WRK-FIM-RUEC09FS
AC0613                                          WRK-FIM-RUEC09GS
AC0613                                          WRK-FIM-RUEC09HS
AC0613                                          WRK-FIM-RUEC09IS.

           PERFORM 2100-OPEN-CSR01-RUECB008.
           PERFORM 2110-FETCH-CSR01-RUECB008
             UNTIL WRK-FIM-RUEC008S    EQUAL 'S'.
           PERFORM 2120-CLOSE-CSR01-RUECB008.

           PERFORM 2200-OPEN-CSR02-RUECB009.
           PERFORM 2210-FETCH-CSR02-RUECB009
             UNTIL WRK-FIM-RUEC009S    EQUAL 'S'.
           PERFORM 2220-CLOSE-CSR02-RUECB009.

           PERFORM 2300-OPEN-CSR03-RUECV010.
           PERFORM 2310-FETCH-CSR03-RUECV010
             UNTIL WRK-FIM-RUEC010S    EQUAL 'S'.
           PERFORM 2320-CLOSE-CSR03-RUECV010.

           PERFORM 2400-OPEN-CSR04-RUECV011.
           PERFORM 2410-FETCH-CSR04-RUECV011
             UNTIL WRK-FIM-RUEC011S    EQUAL 'S'.
           PERFORM 2420-CLOSE-CSR04-RUECV011.

           PERFORM 2900-OPEN-CSR09-RUECV019.
           PERFORM 2910-FETCH-CSR09-RUECV019
             UNTIL WRK-FIM-RUEC019S    EQUAL 'S'.
           PERFORM 2920-CLOSE-CSR09-RUECV019.

           PERFORM 3300-OPEN-CSR13-RUECV025.
           PERFORM 3310-FETCH-CSR13-RUECV025
             UNTIL WRK-FIM-RUEC025S    EQUAL 'S'.
           PERFORM 3320-CLOSE-CSR13-RUECV025.

           PERFORM 3400-OPEN-CSR14-RUECV026.
           PERFORM 3410-FETCH-CSR14-RUECV026
             UNTIL WRK-FIM-RUEC026S    EQUAL 'S'.
           PERFORM 3420-CLOSE-CSR14-RUECV026.

           PERFORM 3500-OPEN-CSR15-RUECB027.
           PERFORM 3510-FETCH-CSR15-RUECB027
             UNTIL WRK-FIM-RUEC027S    EQUAL 'S'.
           PERFORM 3520-CLOSE-CSR15-RUECB027.

           PERFORM 3700-ACESSAR-RUECB033.

           PERFORM 3800-OPEN-CSR18-RUECB034.
           PERFORM 3810-FETCH-CSR18-RUECB034
             UNTIL WRK-FIM-RUEC034S    EQUAL 'S'.
           PERFORM 3820-CLOSE-CSR18-RUECB034.

           PERFORM 3900-OPEN-CSR19-RUECB035.
           PERFORM 3910-FETCH-CSR19-RUECB035
             UNTIL WRK-FIM-RUEC035S    EQUAL 'S'.
           PERFORM 3920-CLOSE-CSR19-RUECB035.

           PERFORM 4000-OPEN-CSR20-RUECB036.
           PERFORM 4010-FETCH-CSR20-RUECB036
             UNTIL WRK-FIM-RUEC036S    EQUAL 'S'.
           PERFORM 4020-CLOSE-CSR20-RUECB036.

           PERFORM 4300-OPEN-CSR23-RUECB041.
           PERFORM 4310-FETCH-CSR23-RUECB041
             UNTIL WRK-FIM-RUEC041S    EQUAL 'S'.
           PERFORM 4320-CLOSE-CSR23-RUECB041.

           PERFORM 4400-ACESSAR-RUECB044.

           PERFORM 5000-OPEN-CSR30-RUECB066.
           PERFORM 5010-FETCH-CSR30-RUECB066
             UNTIL WRK-FIM-RUEC066S    EQUAL 'S'.
           PERFORM 5020-CLOSE-CSR30-RUECB066.

           PERFORM 5200-OPEN-CSR32-RUECB074.
           PERFORM 5210-FETCH-CSR32-RUECB074
             UNTIL WRK-FIM-RUEC074S    EQUAL 'S'.
           PERFORM 5220-CLOSE-CSR32-RUECB074.

AC0613     PERFORM 5500-OPEN-CSR35-RUECB097.
AC0613     PERFORM 5510-FETCH-CSR35-RUECB097
AC0613       UNTIL WRK-FIM-RUEC097S    EQUAL 'S'.
AC0613     PERFORM 5520-CLOSE-CSR35-RUECB097.
AC0613
AC0613     PERFORM 6300-OPEN-CSR43-RUECB09H.
AC0613     PERFORM 6310-FETCH-CSR43-RUECB09H
AC0613       UNTIL WRK-FIM-RUEC09HS    EQUAL 'S'.
AC0613     PERFORM 6320-CLOSE-CSR43-RUECB09H.

           IF  WRK-B007-CCONTR-NULL    NOT EQUAL '?'

               PERFORM 2500-OPEN-CSR05-RUECV012
               PERFORM 2510-FETCH-CSR05-RUECV012
                 UNTIL WRK-FIM-RUEC012S EQUAL 'S'
               PERFORM 2520-CLOSE-CSR05-RUECV012

               PERFORM 2600-OPEN-CSR06-RUECB014
               PERFORM 2610-FETCH-CSR06-RUECB014
                 UNTIL WRK-FIM-RUEC014S EQUAL 'S'
               PERFORM 2620-CLOSE-CSR06-RUECB014

               PERFORM 2700-OPEN-CSR07-RUECB017
               PERFORM 2710-FETCH-CSR07-RUECB017
                 UNTIL WRK-FIM-RUEC017S EQUAL 'S'
               PERFORM 2720-CLOSE-CSR07-RUECB017

               PERFORM 2800-OPEN-CSR08-RUECV018
               PERFORM 2810-FETCH-CSR08-RUECV018
                 UNTIL WRK-FIM-RUEC018S EQUAL 'S'
               PERFORM 2820-CLOSE-CSR08-RUECV018

               PERFORM 3000-OPEN-CSR10-RUECV022
               PERFORM 3010-FETCH-CSR10-RUECV022
                 UNTIL WRK-FIM-RUEC022S EQUAL 'S'
               PERFORM 3020-CLOSE-CSR10-RUECV022

               PERFORM 3100-OPEN-CSR11-RUECV023
               PERFORM 3110-FETCH-CSR11-RUECV023
                 UNTIL WRK-FIM-RUEC023S EQUAL 'S'
               PERFORM 3120-CLOSE-CSR11-RUECV023

               PERFORM 3200-OPEN-CSR12-RUECB024
               PERFORM 3210-FETCH-CSR12-RUECB024
                 UNTIL WRK-FIM-RUEC024S EQUAL 'S'
               PERFORM 3220-CLOSE-CSR12-RUECB024

               PERFORM 3600-OPEN-CSR16-RUECB028
               PERFORM 3610-FETCH-CSR16-RUECB028
                 UNTIL WRK-FIM-RUEC028S EQUAL 'S'
               PERFORM 3620-CLOSE-CSR16-RUECB028

               PERFORM 4100-OPEN-CSR21-RUECB038
               PERFORM 4110-FETCH-CSR21-RUECB038
                 UNTIL WRK-FIM-RUEC038S EQUAL 'S'
               PERFORM 4120-CLOSE-CSR21-RUECB038

               PERFORM 4200-OPEN-CSR22-RUECB040
               PERFORM 4210-FETCH-CSR22-RUECB040
                 UNTIL WRK-FIM-RUEC040S EQUAL 'S'
               PERFORM 4220-CLOSE-CSR22-RUECB040

               PERFORM 4500-OPEN-CSR25-RUECB052
               PERFORM 4510-FETCH-CSR25-RUECB052
                 UNTIL WRK-FIM-RUEC052S EQUAL 'S'
               PERFORM 4520-CLOSE-CSR25-RUECB052

               PERFORM 4600-OPEN-CSR26-RUECB053
               PERFORM 4610-FETCH-CSR26-RUECB053
                 UNTIL WRK-FIM-RUEC053S EQUAL 'S'
               PERFORM 4620-CLOSE-CSR26-RUECB053

               PERFORM 4700-OPEN-CSR27-RUECB054
               PERFORM 4710-FETCH-CSR27-RUECB054
                 UNTIL WRK-FIM-RUEC054S EQUAL 'S'
               PERFORM 4720-CLOSE-CSR27-RUECB054

               PERFORM 4800-OPEN-CSR28-RUECB064
               PERFORM 4810-FETCH-CSR28-RUECB064
                 UNTIL WRK-FIM-RUEC064S EQUAL 'S'
               PERFORM 4820-CLOSE-CSR28-RUECB064

               PERFORM 4900-OPEN-CSR29-RUECB065
               PERFORM 4910-FETCH-CSR29-RUECB065
                 UNTIL WRK-FIM-RUEC065S EQUAL 'S'
               PERFORM 4920-CLOSE-CSR29-RUECB065

               PERFORM 5100-OPEN-CSR31-RUECB073
               PERFORM 5110-FETCH-CSR31-RUECB073
                 UNTIL WRK-FIM-RUEC073S EQUAL 'S'
               PERFORM 5120-CLOSE-CSR31-RUECB073

               PERFORM 5300-OPEN-CSR33-RUECB075
               PERFORM 5310-FETCH-CSR33-RUECB075
                 UNTIL WRK-FIM-RUEC075S EQUAL 'S'
               PERFORM 5320-CLOSE-CSR33-RUECB075

               PERFORM 5400-OPEN-CSR34-RUECB096
               PERFORM 5410-FETCH-CSR34-RUECB096
                 UNTIL WRK-FIM-RUEC096S EQUAL 'S'
               PERFORM 5420-CLOSE-CSR34-RUECB096
AC0613
AC0613         PERFORM 5600-OPEN-CSR36-RUECB099
AC0613         PERFORM 5610-FETCH-CSR36-RUECB099
AC0613           UNTIL WRK-FIM-RUEC099S EQUAL 'S'
AC0613         PERFORM 5620-CLOSE-CSR36-RUECB099
AC0613
AC0613         PERFORM 5700-OPEN-CSR37-RUECB09A
AC0613         PERFORM 5710-FETCH-CSR37-RUECB09A
AC0613           UNTIL WRK-FIM-RUEC09AS EQUAL 'S'
AC0613         PERFORM 5720-CLOSE-CSR37-RUECB09A
AC0613
AC0613         PERFORM 5800-OPEN-CSR38-RUECB09B
AC0613         PERFORM 5810-FETCH-CSR38-RUECB09B
AC0613           UNTIL WRK-FIM-RUEC09BS EQUAL 'S'
AC0613         PERFORM 5820-CLOSE-CSR38-RUECB09B
AC0613
AC0613         PERFORM 5900-OPEN-CSR39-RUECB09D
AC0613         PERFORM 5910-FETCH-CSR39-RUECB09D
AC0613           UNTIL WRK-FIM-RUEC09DS EQUAL 'S'
AC0613         PERFORM 5920-CLOSE-CSR39-RUECB09D
AC0613
AC0613         PERFORM 6000-OPEN-CSR40-RUECB09E
AC0613         PERFORM 6010-FETCH-CSR40-RUECB09E
AC0613           UNTIL WRK-FIM-RUEC09ES EQUAL 'S'
AC0613         PERFORM 6020-CLOSE-CSR40-RUECB09E
AC0613
AC0613         PERFORM 6100-OPEN-CSR41-RUECB09F
AC0613         PERFORM 6110-FETCH-CSR41-RUECB09F
AC0613           UNTIL WRK-FIM-RUEC09FS EQUAL 'S'
AC0613         PERFORM 6120-CLOSE-CSR41-RUECB09F
AC0613
AC0613         PERFORM 6200-OPEN-CSR42-RUECB09G
AC0613         PERFORM 6210-FETCH-CSR42-RUECB09G
AC0613           UNTIL WRK-FIM-RUEC09GS EQUAL 'S'
AC0613         PERFORM 6220-CLOSE-CSR42-RUECB09G
AC0613
AC0613         PERFORM 6400-OPEN-CSR44-RUECB09I
AC0613         PERFORM 6410-FETCH-CSR44-RUECB09I
AC0613           UNTIL WRK-FIM-RUEC09IS EQUAL 'S'
AC0613         PERFORM 6420-CLOSE-CSR44-RUECB09I

           END-IF.

           PERFORM 1400-LER-SELCDULA.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *---------------------------------------------------------------*
       2050-TOTAL-AGENCIA-LIDA         SECTION.
      *---------------------------------------------------------------*

           DISPLAY ' '.
           DISPLAY '|RUEC1450| AGENCIA: ' WRK-CJUNC-DEPDC-ANT
           ' | REGISTROS LIDOS: ' ACU-LIDOS-AGENCIA ' |'.
           DISPLAY '|REGISTROS GRAV. RUECB008: ' ACU-GRAG-RUEC008S.
           DISPLAY '|REGISTROS GRAV. RUECB009: ' ACU-GRAG-RUEC009S.
           DISPLAY '|REGISTROS GRAV. RUECV010: ' ACU-GRAG-RUEC010S.
           DISPLAY '|REGISTROS GRAV. RUECV011: ' ACU-GRAG-RUEC011S.
           DISPLAY '|REGISTROS GRAV. RUECV012: ' ACU-GRAG-RUEC012S.
           DISPLAY '|REGISTROS GRAV. RUECB014: ' ACU-GRAG-RUEC014S.
           DISPLAY '|REGISTROS GRAV. RUECB017: ' ACU-GRAG-RUEC017S.
           DISPLAY '|REGISTROS GRAV. RUECV018: ' ACU-GRAG-RUEC018S.
           DISPLAY '|REGISTROS GRAV. RUECV019: ' ACU-GRAG-RUEC019S.
           DISPLAY '|REGISTROS GRAV. RUECV022: ' ACU-GRAG-RUEC022S.
           DISPLAY '|REGISTROS GRAV. RUECV023: ' ACU-GRAG-RUEC023S.
           DISPLAY '|REGISTROS GRAV. RUECB024: ' ACU-GRAG-RUEC024S.
           DISPLAY '|REGISTROS GRAV. RUECB025: ' ACU-GRAG-RUEC025S.
           DISPLAY '|REGISTROS GRAV. RUECV026: ' ACU-GRAG-RUEC026S.
           DISPLAY '|REGISTROS GRAV. RUECB027: ' ACU-GRAG-RUEC027S.
           DISPLAY '|REGISTROS GRAV. RUECB028: ' ACU-GRAG-RUEC028S.
           DISPLAY '|REGISTROS GRAV. RUECB033: ' ACU-GRAG-RUEC033S.
           DISPLAY '|REGISTROS GRAV. RUECB034: ' ACU-GRAG-RUEC034S.
           DISPLAY '|REGISTROS GRAV. RUECB035: ' ACU-GRAG-RUEC035S.
           DISPLAY '|REGISTROS GRAV. RUECB036: ' ACU-GRAG-RUEC036S.
           DISPLAY '|REGISTROS GRAV. RUECB038: ' ACU-GRAG-RUEC038S.
           DISPLAY '|REGISTROS GRAV. RUECB040: ' ACU-GRAG-RUEC040S.
           DISPLAY '|REGISTROS GRAV. RUECB041: ' ACU-GRAG-RUEC041S.
           DISPLAY '|REGISTROS GRAV. RUECB044: ' ACU-GRAG-RUEC044S.
           DISPLAY '|REGISTROS GRAV. RUECB052: ' ACU-GRAG-RUEC052S.
           DISPLAY '|REGISTROS GRAV. RUECB053: ' ACU-GRAG-RUEC053S.
           DISPLAY '|REGISTROS GRAV. RUECB054: ' ACU-GRAG-RUEC054S.
           DISPLAY '|REGISTROS GRAV. RUECB064: ' ACU-GRAG-RUEC064S.
           DISPLAY '|REGISTROS GRAV. RUECB065: ' ACU-GRAG-RUEC065S.
           DISPLAY '|REGISTROS GRAV. RUECB066: ' ACU-GRAG-RUEC066S.
           DISPLAY '|REGISTROS GRAV. RUECB073: ' ACU-GRAG-RUEC073S.
           DISPLAY '|REGISTROS GRAV. RUECB074: ' ACU-GRAG-RUEC074S.
           DISPLAY '|REGISTROS GRAV. RUECB075: ' ACU-GRAG-RUEC075S.
           DISPLAY '|REGISTROS GRAV. RUECB096: ' ACU-GRAG-RUEC096S.
AC0613     DISPLAY '|REGISTROS GRAV. RUECB097: ' ACU-GRAG-RUEC097S.
AC0613     DISPLAY '|REGISTROS GRAV. RUECB099: ' ACU-GRAG-RUEC099S.
AC0613     DISPLAY '|REGISTROS GRAV. RUECB09A: ' ACU-GRAG-RUEC09AS.
AC0613     DISPLAY '|REGISTROS GRAV. RUECB09B: ' ACU-GRAG-RUEC09BS.
AC0613     DISPLAY '|REGISTROS GRAV. RUECB09D: ' ACU-GRAG-RUEC09DS.
AC0613     DISPLAY '|REGISTROS GRAV. RUECB09E: ' ACU-GRAG-RUEC09ES.
AC0613     DISPLAY '|REGISTROS GRAV. RUECB09F: ' ACU-GRAG-RUEC09FS.
AC0613     DISPLAY '|REGISTROS GRAV. RUECB09G: ' ACU-GRAG-RUEC09GS.
AC0613     DISPLAY '|REGISTROS GRAV. RUECB09H: ' ACU-GRAG-RUEC09HS.
AC0613     DISPLAY '|REGISTROS GRAV. RUECB09I: ' ACU-GRAG-RUEC09IS.

           MOVE ZEROS                        TO  ACU-LIDOS-AGENCIA
                                                 ACU-GRAG-RUEC008S
                                                 ACU-GRAG-RUEC009S
                                                 ACU-GRAG-RUEC010S
                                                 ACU-GRAG-RUEC011S
                                                 ACU-GRAG-RUEC012S
                                                 ACU-GRAG-RUEC014S
                                                 ACU-GRAG-RUEC017S
                                                 ACU-GRAG-RUEC018S
                                                 ACU-GRAG-RUEC019S
                                                 ACU-GRAG-RUEC022S
                                                 ACU-GRAG-RUEC023S
                                                 ACU-GRAG-RUEC024S
                                                 ACU-GRAG-RUEC025S
                                                 ACU-GRAG-RUEC026S
                                                 ACU-GRAG-RUEC027S
                                                 ACU-GRAG-RUEC028S
                                                 ACU-GRAG-RUEC033S
                                                 ACU-GRAG-RUEC034S
                                                 ACU-GRAG-RUEC035S
                                                 ACU-GRAG-RUEC036S
                                                 ACU-GRAG-RUEC038S
                                                 ACU-GRAG-RUEC040S
                                                 ACU-GRAG-RUEC041S
                                                 ACU-GRAG-RUEC044S
                                                 ACU-GRAG-RUEC052S
                                                 ACU-GRAG-RUEC053S
                                                 ACU-GRAG-RUEC054S
                                                 ACU-GRAG-RUEC064S
                                                 ACU-GRAG-RUEC065S
                                                 ACU-GRAG-RUEC066S
                                                 ACU-GRAG-RUEC073S
                                                 ACU-GRAG-RUEC074S
                                                 ACU-GRAG-RUEC075S
                                                 ACU-GRAG-RUEC096S
AC0613                                           ACU-GRAG-RUEC097S
AC0613                                           ACU-GRAG-RUEC099S
AC0613                                           ACU-GRAG-RUEC09AS
AC0613                                           ACU-GRAG-RUEC09BS
AC0613                                           ACU-GRAG-RUEC09DS
AC0613                                           ACU-GRAG-RUEC09ES
AC0613                                           ACU-GRAG-RUEC09FS
AC0613                                           ACU-GRAG-RUEC09GS
AC0613                                           ACU-GRAG-RUEC09HS
AC0613                                           ACU-GRAG-RUEC09IS.

      *---------------------------------------------------------------*
       2050-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *----------------------------------------------------------------*
       2100-OPEN-CSR01-RUECB008        SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             OPEN CSR01-RUECB008
           END-EXEC.

           IF  ( SQLCODE               NOT EQUAL ZEROS             )
           OR  ( SQLWARN0              EQUAL    'W'                )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'RUECB008'         TO  ERR-DBD-TAB
               MOVE 'OPEN    '         TO  ERR-FUN-COMANDO
               MOVE SQLCODE            TO  ERR-SQL-CODE
               MOVE '2100'             TO  ERR-LOCAL
               MOVE SPACES             TO  ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2110-FETCH-CSR01-RUECB008       SECTION.
      *----------------------------------------------------------------*

           INITIALIZE WRK-RUEC008S-NULL
                     REPLACING ALPHANUMERIC DATA BY SPACES.

           INITIALIZE RUECB008.


           EXEC SQL
             FETCH CSR01-RUECB008
             INTO
               :RUECB008.CJUNC-DEPDC,
               :RUECB008.CCDULA,
               :RUECB008.CSEQ-PSSOA,
               :RUECB008.CTPO-RESP-PSSOA,
               :RUECB008.IPSSOA,
               :RUECB008.CTPO-PSSOA,
               :RUECB008.CCGC-CPF,
               :RUECB008.CFLIAL-CGC,
               :RUECB008.CCTRL-CPF-CGC,
               :RUECB008.VRESP-RURAL
                 :WRK-B008-VRESP-RURAL-N,
               :RUECB008.VRESP-GRAL
                 :WRK-B008-VRESP-GRAL-N,
               :RUECB008.VSDO-MED-TMES
                 :WRK-B008-VSDO-MED-TMES-N,
               :RUECB008.VSDO-MED-SEMTL
                 :WRK-B008-VSDO-MED-SEMTL-N,
               :RUECB008.VRENDA-AGROP-OUTRO
                 :WRK-B008-VRENDA-AGROP-OUTRO-N,
               :RUECB008.VRENDA-NAO-AGROP
                 :WRK-B008-VRENDA-NAO-AGROP-N,
               :RUECB008.DCAD
                 :WRK-B008-DCAD-N,
               :RUECB008.DANO-RENDM
                 :WRK-B008-DANO-RENDM-N,
               :RUECB008.CSEQ-HIPOT-RURAL
                 :WRK-B008-CSEQ-HIPOT-RURAL-N,
               :RUECB008.CTPO-DOCTO-PSSOA
                 :WRK-B008-CTPO-DOCTO-PSSOA-N,
               :RUECB008.CDOCTO-PSSOA
                 :WRK-B008-CDOCTO-PSSOA-N,
               :RUECB008.CORG-EMISR-DOCTO
                 :WRK-B008-CORG-EMISR-DOCTO-N,
               :RUECB008.CUND-ORG-EMISR
                 :WRK-B008-CUND-ORG-EMISR-N,
               :RUECB008.CSEXO
                 :WRK-B008-CSEXO-N,
               :RUECB008.CEST-CVIL-RGIME
                 :WRK-B008-CEST-CVIL-RGIME-N,
               :RUECB008.DEST-CVIL
                 :WRK-B008-DEST-CVIL-N,
               :RUECB008.CNAC
                 :WRK-B008-CNAC-N,
               :RUECB008.CSGL-UF
                 :WRK-B008-CSGL-UF-N,
               :RUECB008.INAT-ESTRG
                 :WRK-B008-INAT-ESTRG-N,
               :RUECB008.ELOGDR,
               :RUECB008.ENRO-LOGDR,
               :RUECB008.ECOMPL-LOGDR
                 :WRK-B008-ECOMPL-LOGDR-N,
               :RUECB008.EBAIRO-LOGDR,
               :RUECB008.CCEP,
               :RUECB008.CCEP-COMPL,
               :RUECB008.CMUN-RURAL,
               :RUECB008.CCATEG-PROFS-CREDT
                 :WRK-B008-CCATEG-PROFS-CREDT-N,
               :RUECB008.CPROFS-PSSOA
                 :WRK-B008-CPROFS-PSSOA-N,
               :RUECB008.CCLASS-ATVDD-ECONC
                 :WRK-B008-CCLASS-ATVDD-ECONC-N,
               :RUECB008.CRAMO-ATVDD-ECONC
                 :WRK-B008-CRAMO-ATVDD-ECONC-N,
               :RUECB008.CSRAMO-ATVDD-ECONC
                 :WRK-B008-CSRAMO-ATVDD-ECONC-N,
               :RUECB008.CATVDD-ECONC
                 :WRK-B008-CATVDD-ECONC-N
           END-EXEC.


           IF  ( SQLCODE               NOT EQUAL ZEROS AND +100    )
           OR  ( SQLWARN0              EQUAL    'W'                )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'RUECB008'         TO  ERR-DBD-TAB
               MOVE 'FETCH   '         TO  ERR-FUN-COMANDO
               MOVE SQLCODE            TO  ERR-SQL-CODE
               MOVE '2110'             TO  ERR-LOCAL
               MOVE SPACES             TO  ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO  WRK-FIM-RUEC008S
               GO                      TO  2110-99-FIM
           END-IF.

           ADD 1                       TO ACU-LIDOS-RUEC008S.


           IF  WRK-B008-VRESP-RURAL-N   NOT EQUAL ZEROS
               MOVE '?'                 TO  WRK-B008-VRESP-RURAL-NULL
           END-IF.

           IF  WRK-B008-VRESP-GRAL-N    NOT EQUAL ZEROS
               MOVE '?'                 TO  WRK-B008-VRESP-GRAL-NULL
           END-IF.

           IF  WRK-B008-VSDO-MED-TMES-N  NOT EQUAL ZEROS
               MOVE '?'                 TO  WRK-B008-VSDO-MED-TMES-NULL
           END-IF.

           IF  WRK-B008-VSDO-MED-SEMTL-N   NOT EQUAL ZEROS
               MOVE '?'                TO  WRK-B008-VSDO-MED-SEMTL-NULL
           END-IF.

           IF  WRK-B008-VRENDA-AGROP-OUTRO-N   NOT EQUAL ZEROS
               MOVE '?'              TO  WRK-B008-VRENDA-AGROP-OUTRO-NU
           END-IF.

           IF  WRK-B008-VRENDA-NAO-AGROP-N   NOT EQUAL ZEROS
               MOVE '?'              TO  WRK-B008-VRENDA-NAO-AGROP-NULL
           END-IF.

           IF  WRK-B008-DCAD-N          NOT EQUAL ZEROS
               MOVE '?'                 TO  WRK-B008-DCAD-NULL
           END-IF.

           IF  WRK-B008-DANO-RENDM-N    NOT EQUAL ZEROS
               MOVE '?'                 TO  WRK-B008-DANO-RENDM-NULL
           END-IF.

           IF  WRK-B008-CSEQ-HIPOT-RURAL-N   NOT EQUAL ZEROS
               MOVE '?'             TO  WRK-B008-CSEQ-HIPOT-RURAL-NULL
           END-IF.

           IF  WRK-B008-CTPO-DOCTO-PSSOA-N   NOT EQUAL ZEROS
               MOVE '?'             TO  WRK-B008-CTPO-DOCTO-PSSOA-NULL
           END-IF.

           IF  WRK-B008-CDOCTO-PSSOA-N   NOT EQUAL ZEROS
               MOVE '?'                 TO  WRK-B008-CDOCTO-PSSOA-NULL
           END-IF.

           IF  WRK-B008-CORG-EMISR-DOCTO-N   NOT EQUAL ZEROS
               MOVE '?'             TO  WRK-B008-CORG-EMISR-DOCTO-NULL
           END-IF.

           IF  WRK-B008-CUND-ORG-EMISR-N   NOT EQUAL ZEROS
               MOVE '?'               TO  WRK-B008-CUND-ORG-EMISR-NULL
           END-IF.

           IF  WRK-B008-CSEXO-N         NOT EQUAL ZEROS
               MOVE '?'                 TO  WRK-B008-CSEXO-NULL
           END-IF.

           IF  WRK-B008-CEST-CVIL-RGIME-N   NOT EQUAL ZEROS
               MOVE '?'              TO  WRK-B008-CEST-CVIL-RGIME-NULL
           END-IF.

           IF  WRK-B008-DEST-CVIL-N   NOT EQUAL ZEROS
               MOVE '?'                 TO  WRK-B008-DEST-CVIL-NULL
           END-IF.

           IF  WRK-B008-CNAC-N          NOT EQUAL ZEROS
               MOVE '?'                 TO  WRK-B008-CNAC-NULL
           END-IF.

           IF  WRK-B008-CSGL-UF-N       NOT EQUAL ZEROS
               MOVE '?'                 TO  WRK-B008-CSGL-UF-NULL
           END-IF.

           IF  WRK-B008-INAT-ESTRG-N    NOT EQUAL ZEROS
               MOVE '?'                 TO  WRK-B008-INAT-ESTRG-NULL
           END-IF.

           IF  WRK-B008-ECOMPL-LOGDR-N  NOT EQUAL ZEROS
               MOVE '?'                 TO  WRK-B008-ECOMPL-LOGDR-NULL
           END-IF.

           IF  WRK-B008-CCATEG-PROFS-CREDT-N   NOT EQUAL ZEROS
               MOVE '?'             TO  WRK-B008-CCATEG-PROFS-CREDT-NU
           END-IF.

           IF  WRK-B008-CPROFS-PSSOA-N  NOT EQUAL ZEROS
               MOVE '?'                 TO  WRK-B008-CPROFS-PSSOA-NULL
           END-IF.

           IF  WRK-B008-CCLASS-ATVDD-ECONC-N   NOT EQUAL ZEROS
               MOVE '?'             TO  WRK-B008-CCLASS-ATVDD-ECONC-NU
           END-IF.

           IF  WRK-B008-CRAMO-ATVDD-ECONC-N   NOT EQUAL ZEROS
               MOVE '?'             TO  WRK-B008-CRAMO-ATVDD-ECONC-NUL
           END-IF.

           IF  WRK-B008-CSRAMO-ATVDD-ECONC-N   NOT EQUAL ZEROS
               MOVE '?'             TO  WRK-B008-CSRAMO-ATVDD-ECONC-NU
           END-IF.

           IF  WRK-B008-CATVDD-ECONC-N   NOT EQUAL ZEROS
               MOVE '?'                 TO  WRK-B008-CATVDD-ECONC-NULL
           END-IF.


           STRING  RUECB008 (1:90)
                   WRK-B008-VRESP-RURAL-NULL
                   RUECB008 (91:08)
                   WRK-B008-VRESP-GRAL-NULL
                   RUECB008 (99:08)
                   WRK-B008-VSDO-MED-TMES-NULL
                   RUECB008 (107:08)
                   WRK-B008-VSDO-MED-SEMTL-NULL
                   RUECB008 (115:08)
                   WRK-B008-VRENDA-AGROP-OUTRO-NU
                   RUECB008 (123:08)
                   WRK-B008-VRENDA-NAO-AGROP-NULL
                   RUECB008 (131:10)
                   WRK-B008-DCAD-NULL
                   RUECB008 (141:03)
                   WRK-B008-DANO-RENDM-NULL
                   RUECB008 (144:02)
                   WRK-B008-CSEQ-HIPOT-RURAL-NULL
                   RUECB008 (146:02)
                   WRK-B008-CTPO-DOCTO-PSSOA-NULL
                   RUECB008 (148:15)
                   WRK-B008-CDOCTO-PSSOA-NULL
                   RUECB008 (163:04)
                   WRK-B008-CORG-EMISR-DOCTO-NULL
                   RUECB008 (167:02)
                   WRK-B008-CUND-ORG-EMISR-NULL
                   RUECB008 (169:01)
                   WRK-B008-CSEXO-NULL
                   RUECB008 (170:02)
                   WRK-B008-CEST-CVIL-RGIME-NULL
                   RUECB008 (172:10)
                   WRK-B008-DEST-CVIL-NULL
                   RUECB008 (182:02)
                   WRK-B008-CNAC-NULL
                   RUECB008 (184:02)
                   WRK-B008-CSGL-UF-NULL
                   RUECB008 (186:40)
                   WRK-B008-INAT-ESTRG-NULL
                   RUECB008 (226:60)
                   WRK-B008-ECOMPL-LOGDR-NULL
                   RUECB008 (286:33)
                   WRK-B008-CCATEG-PROFS-CREDT-NU
                   RUECB008 (319:03)
                   WRK-B008-CPROFS-PSSOA-NULL
                   RUECB008 (322:02)
                   WRK-B008-CCLASS-ATVDD-ECONC-NU
                   RUECB008 (324:02)
                   WRK-B008-CRAMO-ATVDD-ECONC-NUL
                   RUECB008 (326:02)
                   WRK-B008-CSRAMO-ATVDD-ECONC-NU
                   RUECB008 (328:02)
                   WRK-B008-CATVDD-ECONC-NULL
                   DELIMITED BY SIZE
           INTO    FD-REG-RUEC008S.


           PERFORM 7030-GRAVAR-SAIDA-RUEC008S.

      *----------------------------------------------------------------*
       2110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2120-CLOSE-CSR01-RUECB008       SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
             CLOSE CSR01-RUECB008
           END-EXEC.

           IF  ( SQLCODE               NOT EQUAL ZEROS             )
           OR  ( SQLWARN0              EQUAL    'W'                )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'RUECB008'         TO  ERR-DBD-TAB
               MOVE 'CLOSE   '         TO  ERR-FUN-COMANDO
               MOVE SQLCODE            TO  ERR-SQL-CODE
               MOVE '2120'             TO  ERR-LOCAL
               MOVE SPACES             TO  ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       2120-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       2200-OPEN-CSR02-RUECB009        SECTION.
      *---------------------------------------------------------------*

           EXEC SQL
             OPEN CSR02-RUECB009
           END-EXEC.

           IF  ( SQLCODE               NOT EQUAL ZEROS             )
           OR  ( SQLWARN0              EQUAL    'W'                )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'RUECB009'         TO  ERR-DBD-TAB
               MOVE 'OPEN    '         TO  ERR-FUN-COMANDO
               MOVE SQLCODE            TO  ERR-SQL-CODE
               MOVE '2200'             TO  ERR-LOCAL
               MOVE SPACES             TO  ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       2210-FETCH-CSR02-RUECB009       SECTION.
      *---------------------------------------------------------------*

           INITIALIZE WRK-RUEC009S-NULL
                     REPLACING ALPHANUMERIC DATA BY SPACES.

           INITIALIZE RUECB009.


           EXEC SQL
             FETCH CSR02-RUECB009
             INTO
               :RUECB009.CJUNC-DEPDC,
               :RUECB009.CCDULA,
               :RUECB009.CSEQ-GARNT,
               :RUECB009.RGARNT-CREDT-RURAL,
               :RUECB009.QGARNT,
               :RUECB009.RQUALF-GARNT,
               :RUECB009.RCOMPL-QUALF-GARNT
                 :WRK-B009-RCOMPL-QUALF-GARNT-N,
               :RUECB009.VGARNT,
               :RUECB009.ILOC-GARNT,
               :RUECB009.CMUN-RURAL
           END-EXEC.

           IF  ( SQLCODE               NOT EQUAL ZEROS AND +100    )
           OR  ( SQLWARN0              EQUAL    'W'                )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'RUECB009'         TO  ERR-DBD-TAB
               MOVE 'FETCH   '         TO  ERR-FUN-COMANDO
               MOVE SQLCODE            TO  ERR-SQL-CODE
               MOVE '2210'             TO  ERR-LOCAL
               MOVE SPACES             TO  ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO  WRK-FIM-RUEC009S
               GO                      TO  2210-99-FIM
           END-IF.

           ADD 1                       TO ACU-LIDOS-RUEC009S.


           IF  WRK-B009-RCOMPL-QUALF-GARNT-N   NOT EQUAL ZEROS
               MOVE '?'             TO  WRK-B009-RCOMPL-QUALF-GARNT-NU
           END-IF.



           STRING  RUECB009 (01:316)
                   WRK-B009-RCOMPL-QUALF-GARNT-NU
                   RUECB009 (317:213)
                   DELIMITED BY SIZE
           INTO    FD-REG-RUEC009S.


           PERFORM 7040-GRAVAR-SAIDA-RUEC009S.

      *---------------------------------------------------------------*
       2210-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       2220-CLOSE-CSR02-RUECB009       SECTION.
      *---------------------------------------------------------------*

           EXEC SQL
             CLOSE CSR02-RUECB009
           END-EXEC.

           IF  ( SQLCODE               NOT EQUAL ZEROS             )
           OR  ( SQLWARN0              EQUAL    'W'                )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'RUECB009'         TO  ERR-DBD-TAB
               MOVE 'CLOSE   '         TO  ERR-FUN-COMANDO
               MOVE SQLCODE            TO  ERR-SQL-CODE
               MOVE '2220'             TO  ERR-LOCAL
               MOVE SPACES             TO  ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       2220-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       2300-OPEN-CSR03-RUECV010        SECTION.
      *---------------------------------------------------------------*

           EXEC SQL
             OPEN CSR03-RUECV010
           END-EXEC.

           IF  ( SQLCODE               NOT EQUAL ZEROS             )
           OR  ( SQLWARN0              EQUAL    'W'                )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'RUECV010'         TO  ERR-DBD-TAB
               MOVE 'OPEN    '         TO  ERR-FUN-COMANDO
               MOVE SQLCODE            TO  ERR-SQL-CODE
               MOVE '2300'             TO  ERR-LOCAL
               MOVE SPACES             TO  ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       2300-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       2310-FETCH-CSR03-RUECV010       SECTION.
      *---------------------------------------------------------------*

           INITIALIZE WRK-RUEC010S-NULL
                     REPLACING ALPHANUMERIC DATA BY SPACES.

           INITIALIZE RUECV010.


           EXEC SQL
             FETCH CSR03-RUECV010
             INTO
               :RUECV010.CJUNC-DEPDC,
               :RUECV010.CCDULA,
               :RUECV010.CTPO-EVNTO-RURAL,
               :RUECV010.DEVNTO,
               :RUECV010.VEVNTO-RURAL,
               :RUECV010.VPCELA-REC-PPRIO
                 :WRK-V010-VPCELA-REC-PPRIO-N
           END-EXEC.

           IF  ( SQLCODE               NOT EQUAL ZEROS AND +100    )
           OR  ( SQLWARN0              EQUAL    'W'                )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'RUECV010'         TO  ERR-DBD-TAB
               MOVE 'FETCH   '         TO  ERR-FUN-COMANDO
               MOVE SQLCODE            TO  ERR-SQL-CODE
               MOVE '2310'             TO  ERR-LOCAL
               MOVE SPACES             TO  ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO  WRK-FIM-RUEC010S
               GO                      TO  2310-99-FIM
           END-IF.

           ADD 1                       TO ACU-LIDOS-RUEC010S.


           IF  WRK-V010-VPCELA-REC-PPRIO-N  NOT EQUAL ZEROS
               MOVE '?'             TO  WRK-V010-VPCELA-REC-PPRIO-NULL
           END-IF.



           STRING  RUECV010 (1:37)
                   WRK-V010-VPCELA-REC-PPRIO-NULL
                   DELIMITED BY SIZE
           INTO    FD-REG-RUEC010S.



           PERFORM 7050-GRAVAR-SAIDA-RUEC010S.

      *---------------------------------------------------------------*
       2310-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       2320-CLOSE-CSR03-RUECV010       SECTION.
      *---------------------------------------------------------------*

           EXEC SQL
             CLOSE CSR03-RUECV010
           END-EXEC.

           IF  ( SQLCODE               NOT EQUAL ZEROS             )
           OR  ( SQLWARN0              EQUAL    'W'                )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'RUECV010'         TO  ERR-DBD-TAB
               MOVE 'CLOSE   '         TO  ERR-FUN-COMANDO
               MOVE SQLCODE            TO  ERR-SQL-CODE
               MOVE '2320'             TO  ERR-LOCAL
               MOVE SPACES             TO  ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       2320-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       2400-OPEN-CSR04-RUECV011        SECTION.
      *---------------------------------------------------------------*

           EXEC SQL
             OPEN CSR04-RUECV011
           END-EXEC.

           IF  ( SQLCODE               NOT EQUAL ZEROS             )
           OR  ( SQLWARN0              EQUAL    'W'                )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'RUECV011'         TO  ERR-DBD-TAB
               MOVE 'OPEN    '         TO  ERR-FUN-COMANDO
               MOVE SQLCODE            TO  ERR-SQL-CODE
               MOVE '2400'             TO  ERR-LOCAL
               MOVE SPACES             TO  ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       2400-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       2410-FETCH-CSR04-RUECV011       SECTION.
      *---------------------------------------------------------------*


           EXEC SQL
             FETCH CSR04-RUECV011
             INTO
               :RUECV011.CJUNC-DEPDC,
               :RUECV011.CCDULA,
               :RUECV011.CSEQ-ITEM-RENDA,
               :RUECV011.RITEM-RENDA,
               :RUECV011.QITEM-RENDA,
               :RUECV011.VITEM-RENDA,
               :RUECV011.PRBATE-ITEM-RENDA
           END-EXEC.

           IF  ( SQLCODE               NOT EQUAL ZEROS AND +100    )
           OR  ( SQLWARN0              EQUAL    'W'                )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'RUECV011'         TO  ERR-DBD-TAB
               MOVE 'FETCH   '         TO  ERR-FUN-COMANDO
               MOVE SQLCODE            TO  ERR-SQL-CODE
               MOVE '2410'             TO  ERR-LOCAL
               MOVE SPACES             TO  ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO  WRK-FIM-RUEC011S
               GO                      TO  2410-99-FIM
           END-IF.

           ADD 1                       TO ACU-LIDOS-RUEC011S.


           PERFORM 7060-GRAVAR-SAIDA-RUEC011S.

      *---------------------------------------------------------------*
       2410-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       2420-CLOSE-CSR04-RUECV011       SECTION.
      *---------------------------------------------------------------*

           EXEC SQL
             CLOSE CSR04-RUECV011
           END-EXEC.

           IF  ( SQLCODE               NOT EQUAL ZEROS             )
           OR  ( SQLWARN0              EQUAL    'W'                )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'RUECV011'         TO  ERR-DBD-TAB
               MOVE 'CLOSE   '         TO  ERR-FUN-COMANDO
               MOVE SQLCODE            TO  ERR-SQL-CODE
               MOVE '2420'             TO  ERR-LOCAL
               MOVE SPACES             TO  ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       2420-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       2500-OPEN-CSR05-RUECV012        SECTION.
      *---------------------------------------------------------------*

           EXEC SQL
             OPEN CSR05-RUECV012
           END-EXEC.

           IF  ( SQLCODE               NOT EQUAL ZEROS             )
           OR  ( SQLWARN0              EQUAL    'W'                )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'RUECV012'         TO  ERR-DBD-TAB
               MOVE 'OPEN    '         TO  ERR-FUN-COMANDO
               MOVE SQLCODE            TO  ERR-SQL-CODE
               MOVE '2500'             TO  ERR-LOCAL
               MOVE SPACES             TO  ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       2500-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       2510-FETCH-CSR05-RUECV012       SECTION.
      *---------------------------------------------------------------*


           EXEC SQL
             FETCH CSR05-RUECV012
             INTO
               :RUECV012.CCONTR,
               :RUECV012.CTPO-EVNTO-RURAL,
               :RUECV012.DEVNTO,
               :RUECV012.VEVNTO-RURAL,
               :RUECV012.VPCELA-REC-PPRIO
           END-EXEC.

           IF  ( SQLCODE               NOT EQUAL ZEROS AND +100    )
           OR  ( SQLWARN0              EQUAL    'W'                )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'RUECV012'         TO  ERR-DBD-TAB
               MOVE 'FETCH   '         TO  ERR-FUN-COMANDO
               MOVE SQLCODE            TO  ERR-SQL-CODE
               MOVE '2510'             TO  ERR-LOCAL
               MOVE SPACES             TO  ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO  WRK-FIM-RUEC012S
               GO                      TO  2510-99-FIM
           END-IF.

           ADD 1                       TO ACU-LIDOS-RUEC012S.


           PERFORM 7070-GRAVAR-SAIDA-RUEC012S.

      *---------------------------------------------------------------*
       2510-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       2520-CLOSE-CSR05-RUECV012       SECTION.
      *---------------------------------------------------------------*

           EXEC SQL
             CLOSE CSR05-RUECV012
           END-EXEC.

           IF  ( SQLCODE               NOT EQUAL ZEROS             )
           OR  ( SQLWARN0              EQUAL    'W'                )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'RUECV012'         TO  ERR-DBD-TAB
               MOVE 'CLOSE   '         TO  ERR-FUN-COMANDO
               MOVE SQLCODE            TO  ERR-SQL-CODE
               MOVE '2520'             TO  ERR-LOCAL
               MOVE SPACES             TO  ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       2520-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       2600-OPEN-CSR06-RUECB014        SECTION.
      *---------------------------------------------------------------*

           EXEC SQL
             OPEN CSR06-RUECB014
           END-EXEC.

           IF  ( SQLCODE               NOT EQUAL ZEROS             )
           OR  ( SQLWARN0              EQUAL    'W'                )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'RUECB014'         TO  ERR-DBD-TAB
               MOVE 'OPEN    '         TO  ERR-FUN-COMANDO
               MOVE SQLCODE            TO  ERR-SQL-CODE
               MOVE '2600'             TO  ERR-LOCAL
               MOVE SPACES             TO  ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       2600-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       2610-FETCH-CSR06-RUECB014       SECTION.
      *---------------------------------------------------------------*

           INITIALIZE WRK-RUEC014S-NULL
                     REPLACING ALPHANUMERIC DATA BY SPACES.

           INITIALIZE RUECB014.


           EXEC SQL
             FETCH CSR06-RUECB014
             INTO
               :RUECB014.CCONTR,
               :RUECB014.CSEQ-PSSOA,
               :RUECB014.CTPO-PSSOA,
               :RUECB014.CTPO-RESP-PSSOA,
               :RUECB014.IPSSOA,
               :RUECB014.CCGC-CPF,
               :RUECB014.CFLIAL-CGC,
               :RUECB014.CCTRL-CPF-CGC,
               :RUECB014.VRESP-RURAL
                 :WRK-B014-VRESP-RURAL-N,
               :RUECB014.VRESP-GRAL
                 :WRK-B014-VRESP-GRAL-N,
               :RUECB014.VSDO-MED-TMES
                 :WRK-B014-VSDO-MED-TMES-N,
               :RUECB014.VSDO-MED-SEMTL
                 :WRK-B014-VSDO-MED-SEMTL-N,
               :RUECB014.VRENDA-AGROP-OUTRO
                 :WRK-B014-VRENDA-AGROP-OUTRO-N,
               :RUECB014.VRENDA-NAO-AGROP
                 :WRK-B014-VRENDA-NAO-AGROP-N,
               :RUECB014.DCAD
                 :WRK-B014-DCAD-N,
               :RUECB014.DANO-RENDM
                 :WRK-B014-DANO-RENDM-N,
               :RUECB014.CSEQ-HIPOT-RURAL
                 :WRK-B014-CSEQ-HIPOT-RURAL-N,
               :RUECB014.CTPO-DOCTO-PSSOA
                 :WRK-B014-CTPO-DOCTO-PSSOA-N,
               :RUECB014.CDOCTO-PSSOA
                 :WRK-B014-CDOCTO-PSSOA-N,
               :RUECB014.CORG-EMISR-DOCTO
                 :WRK-B014-CORG-EMISR-DOCTO-N,
               :RUECB014.CUND-ORG-EMISR
                 :WRK-B014-CUND-ORG-EMISR-N,
               :RUECB014.CSEXO
                 :WRK-B014-CSEXO-N,
               :RUECB014.CEST-CVIL-RGIME
                 :WRK-B014-CEST-CVIL-RGIME-N,
               :RUECB014.DEST-CVIL
                 :WRK-B014-DEST-CVIL-N,
               :RUECB014.CNAC
                 :WRK-B014-CNAC-N,
               :RUECB014.CSGL-UF
                 :WRK-B014-CSGL-UF-N,
               :RUECB014.INAT-ESTRG
                 :WRK-B014-INAT-ESTRG-N,
               :RUECB014.ELOGDR,
               :RUECB014.ENRO-LOGDR,
               :RUECB014.ECOMPL-LOGDR
                 :WRK-B014-ECOMPL-LOGDR-N,
               :RUECB014.EBAIRO-LOGDR,
               :RUECB014.CCEP,
               :RUECB014.CCEP-COMPL,
               :RUECB014.CMUN-RURAL,
               :RUECB014.CCATEG-PROFS-CREDT
                 :WRK-B014-CCATEG-PROFS-CREDT-N,
               :RUECB014.CPROFS-PSSOA
                 :WRK-B014-CPROFS-PSSOA-N,
               :RUECB014.CCLASS-ATVDD-ECONC
                 :WRK-B014-CCLASS-ATVDD-ECONC-N,
               :RUECB014.CRAMO-ATVDD-ECONC
                 :WRK-B014-CRAMO-ATVDD-ECONC-N,
               :RUECB014.CSRAMO-ATVDD-ECONC
                 :WRK-B014-CSRAMO-ATVDD-ECONC-N,
               :RUECB014.CATVDD-ECONC
                 :WRK-B014-CATVDD-ECONC-N
           END-EXEC.

           IF  ( SQLCODE               NOT EQUAL ZEROS AND +100    )
           OR  ( SQLWARN0              EQUAL    'W'                )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'RUECB014'         TO  ERR-DBD-TAB
               MOVE 'FETCH   '         TO  ERR-FUN-COMANDO
               MOVE SQLCODE            TO  ERR-SQL-CODE
               MOVE '2610'             TO  ERR-LOCAL
               MOVE SPACES             TO  ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO  WRK-FIM-RUEC014S
               GO                      TO  2610-99-FIM
           END-IF.

           ADD 1                       TO ACU-LIDOS-RUEC014S.


           IF  WRK-B014-VRESP-RURAL-N    NOT EQUAL ZEROS
               MOVE '?'                  TO  WRK-B014-VRESP-RURAL-NULL
           END-IF.

           IF  WRK-B014-VRESP-GRAL-N     NOT EQUAL ZEROS
               MOVE '?'                  TO  WRK-B014-VRESP-GRAL-NULL
           END-IF.

           IF  WRK-B014-VSDO-MED-TMES-N  NOT EQUAL ZEROS
               MOVE '?'                TO  WRK-B014-VSDO-MED-TMES-NULL
           END-IF.

           IF  WRK-B014-VSDO-MED-SEMTL-N  NOT EQUAL ZEROS
               MOVE '?'               TO  WRK-B014-VSDO-MED-SEMTL-NULL
           END-IF.

           IF  WRK-B014-VRENDA-AGROP-OUTRO-N   NOT EQUAL ZEROS
               MOVE '?'             TO  WRK-B014-VRENDA-AGROP-OUTRO-NU
           END-IF.

           IF  WRK-B014-VRENDA-NAO-AGROP-N  NOT EQUAL ZEROS
               MOVE '?'             TO  WRK-B014-VRENDA-NAO-AGROP-NULL
           END-IF.

           IF  WRK-B014-DCAD-N         NOT EQUAL ZEROS
               MOVE '?'                TO  WRK-B014-DCAD-NULL
           END-IF.

           IF  WRK-B014-DANO-RENDM-N   NOT EQUAL ZEROS
               MOVE '?'                TO  WRK-B014-DANO-RENDM-NULL
           END-IF.

           IF  WRK-B014-CSEQ-HIPOT-RURAL-N  NOT EQUAL ZEROS
               MOVE '?'             TO  WRK-B014-CSEQ-HIPOT-RURAL-NULL
           END-IF.

           IF  WRK-B014-CTPO-DOCTO-PSSOA-N  NOT EQUAL ZEROS
               MOVE '?'             TO  WRK-B014-CTPO-DOCTO-PSSOA-NULL
           END-IF.

           IF  WRK-B014-CDOCTO-PSSOA-N  NOT EQUAL ZEROS
               MOVE '?'                 TO  WRK-B014-CDOCTO-PSSOA-NULL
           END-IF.

           IF  WRK-B014-CORG-EMISR-DOCTO-N  NOT EQUAL ZEROS
               MOVE '?'             TO  WRK-B014-CORG-EMISR-DOCTO-NULL
           END-IF.

           IF  WRK-B014-CUND-ORG-EMISR-N  NOT EQUAL ZEROS
               MOVE '?'               TO  WRK-B014-CUND-ORG-EMISR-NULL
           END-IF.

           IF  WRK-B014-CSEXO-N       NOT EQUAL ZEROS
               MOVE '?'               TO  WRK-B014-CSEXO-NULL
           END-IF.

           IF  WRK-B014-CEST-CVIL-RGIME-N  NOT EQUAL ZEROS
               MOVE '?'              TO  WRK-B014-CEST-CVIL-RGIME-NULL
           END-IF.

           IF  WRK-B014-DEST-CVIL-N   NOT EQUAL ZEROS
               MOVE '?'               TO  WRK-B014-DEST-CVIL-NULL
           END-IF.

           IF  WRK-B014-CNAC-N        NOT EQUAL ZEROS
               MOVE '?'               TO  WRK-B014-CNAC-NULL
           END-IF.

           IF  WRK-B014-CSGL-UF-N     NOT EQUAL ZEROS
               MOVE '?'               TO  WRK-B014-CSGL-UF-NULL
           END-IF.

           IF  WRK-B014-INAT-ESTRG-N  NOT EQUAL ZEROS
               MOVE '?'               TO  WRK-B014-INAT-ESTRG-NULL
           END-IF.

           IF  WRK-B014-ECOMPL-LOGDR-N  NOT EQUAL ZEROS
               MOVE '?'               TO  WRK-B014-ECOMPL-LOGDR-NULL
           END-IF.

           IF  WRK-B014-CCATEG-PROFS-CREDT-N  NOT EQUAL ZEROS
               MOVE '?'             TO  WRK-B014-CCATEG-PROFS-CREDT-NU
           END-IF.

           IF  WRK-B014-CPROFS-PSSOA-N  NOT EQUAL ZEROS
               MOVE '?'               TO  WRK-B014-CPROFS-PSSOA-NULL
           END-IF.

           IF  WRK-B014-CCLASS-ATVDD-ECONC-N  NOT EQUAL ZEROS
               MOVE '?'             TO  WRK-B014-CCLASS-ATVDD-ECONC-NU
           END-IF.

           IF  WRK-B014-CRAMO-ATVDD-ECONC-N  NOT EQUAL ZEROS
               MOVE '?'             TO  WRK-B014-CRAMO-ATVDD-ECONC-NUL
           END-IF.

           IF  WRK-B014-CSRAMO-ATVDD-ECONC-N  NOT EQUAL ZEROS
               MOVE '?'             TO  WRK-B014-CSRAMO-ATVDD-ECONC-NU
           END-IF.

           IF  WRK-B014-CATVDD-ECONC-N  NOT EQUAL ZEROS
               MOVE '?'             TO  WRK-B014-CATVDD-ECONC-NULL
           END-IF.



           STRING  RUECB014 (1:87)
                   WRK-B014-VRESP-RURAL-NULL
                   RUECB014 (88:08)
                   WRK-B014-VRESP-GRAL-NULL
                   RUECB014 (96:08)
                   WRK-B014-VSDO-MED-TMES-NULL
                   RUECB014 (104:08)
                   WRK-B014-VSDO-MED-SEMTL-NULL
                   RUECB014 (112:08)
                   WRK-B014-VRENDA-AGROP-OUTRO-NU
                   RUECB014 (120:08)
                   WRK-B014-VRENDA-NAO-AGROP-NULL
                   RUECB014 (128:10)
                   WRK-B014-DCAD-NULL
                   RUECB014 (138:03)
                   WRK-B014-DANO-RENDM-NULL
                   RUECB014 (141:02)
                   WRK-B014-CSEQ-HIPOT-RURAL-NULL
                   RUECB014 (143:02)
                   WRK-B014-CTPO-DOCTO-PSSOA-NULL
                   RUECB014 (145:15)
                   WRK-B014-CDOCTO-PSSOA-NULL
                   RUECB014 (160:04)
                   WRK-B014-CORG-EMISR-DOCTO-NULL
                   RUECB014 (164:02)
                   WRK-B014-CUND-ORG-EMISR-NULL
                   RUECB014 (166:01)
                   WRK-B014-CSEXO-NULL
                   RUECB014 (167:02)
                   WRK-B014-CEST-CVIL-RGIME-NULL
                   RUECB014 (169:10)
                   WRK-B014-DEST-CVIL-NULL
                   RUECB014 (179:02)
                   WRK-B014-CNAC-NULL
                   RUECB014 (181:02)
                   WRK-B014-CSGL-UF-NULL
                   RUECB014 (183:40)
                   WRK-B014-INAT-ESTRG-NULL
                   RUECB014 (223:60)
                   WRK-B014-ECOMPL-LOGDR-NULL
                   RUECB014 (283:33)
                   WRK-B014-CCATEG-PROFS-CREDT-NU
                   RUECB014 (316:03)
                   WRK-B014-CPROFS-PSSOA-NULL
                   RUECB014 (319:02)
                   WRK-B014-CCLASS-ATVDD-ECONC-NU
                   RUECB014 (321:02)
                   WRK-B014-CRAMO-ATVDD-ECONC-NUL
                   RUECB014 (323:02)
                   WRK-B014-CSRAMO-ATVDD-ECONC-NU
                   RUECB014 (325:02)
                   WRK-B014-CATVDD-ECONC-NULL
                   DELIMITED BY SIZE
           INTO    FD-REG-RUEC014S.


           PERFORM 7080-GRAVAR-SAIDA-RUEC014S.

      *---------------------------------------------------------------*
       2610-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       2620-CLOSE-CSR06-RUECB014       SECTION.
      *---------------------------------------------------------------*

           EXEC SQL
             CLOSE CSR06-RUECB014
           END-EXEC.

           IF  ( SQLCODE               NOT EQUAL ZEROS             )
           OR  ( SQLWARN0              EQUAL    'W'                )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'RUECB014'         TO  ERR-DBD-TAB
               MOVE 'CLOSE   '         TO  ERR-FUN-COMANDO
               MOVE SQLCODE            TO  ERR-SQL-CODE
               MOVE '2620'             TO  ERR-LOCAL
               MOVE SPACES             TO  ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       2620-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       2700-OPEN-CSR07-RUECB017        SECTION.
      *---------------------------------------------------------------*

           EXEC SQL
             OPEN CSR07-RUECB017
           END-EXEC.

           IF  ( SQLCODE               NOT EQUAL ZEROS             )
           OR  ( SQLWARN0              EQUAL    'W'                )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'RUECB017'         TO  ERR-DBD-TAB
               MOVE 'OPEN    '         TO  ERR-FUN-COMANDO
               MOVE SQLCODE            TO  ERR-SQL-CODE
               MOVE '2700'             TO  ERR-LOCAL
               MOVE SPACES             TO  ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       2700-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       2710-FETCH-CSR07-RUECB017       SECTION.
      *---------------------------------------------------------------*

           INITIALIZE WRK-RUEC017S-NULL
                     REPLACING ALPHANUMERIC DATA BY SPACES.

           INITIALIZE RUECB017.


           EXEC SQL
             FETCH CSR07-RUECB017
             INTO
               :RUECB017.CCONTR,
               :RUECB017.CSEQ-HIPOT-RURAL,
               :RUECB017.IPPRIE,
               :RUECB017.RLOC-IMOV,
               :RUECB017.MAREA-IMOV,
               :RUECB017.CUND-MEDD,
               :RUECB017.DAQUIS-IMOV,
               :RUECB017.CFL-LIVRO-REG-IMOV,
               :RUECB017.CLIVRO-REG-IMOV,
               :RUECB017.ICARTR,
               :RUECB017.ICOMAR,
               :RUECB017.IDOCTO-POSSE-IMOV,
               :RUECB017.VAVALC-IMOV
                 :WRK-B017-VAVALC-IMOV-N,
               :RUECB017.CMATR-IMOV-RURAL,
               :RUECB017.CGRAU-PNHOR,
               :RUECB017.CSEQ-ESCRT-ONUS
                 :WRK-B017-CSEQ-ESCRT-ONUS-N,
               :RUECB017.CSEQ-PATRM-IMOV
                 :WRK-B017-CSEQ-PATRM-IMOV-N,
               :RUECB017.CMUN-RURAL,
               :RUECB017.CCARTR-IMOV-REG
                 :WRK-B017-CCARTR-IMOV-REG-N,
               :RUECB017.CGLEBA-IMOV-HIPOT
                 :WRK-B017-CGLEBA-IMOV-HIPOT-N,
               :RUECB017.CLOTE-IMOV-HIPOT
                 :WRK-B017-CLOTE-IMOV-HIPOT-N
           END-EXEC.


           IF  ( SQLCODE               NOT EQUAL ZEROS AND +100    )
           OR  ( SQLWARN0              EQUAL    'W'                )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'RUECB017'         TO  ERR-DBD-TAB
               MOVE 'FETCH   '         TO  ERR-FUN-COMANDO
               MOVE SQLCODE            TO  ERR-SQL-CODE
               MOVE '2710'             TO  ERR-LOCAL
               MOVE SPACES             TO  ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO  WRK-FIM-RUEC017S
               GO                      TO  2710-99-FIM
           END-IF.

           ADD 1                       TO ACU-LIDOS-RUEC017S.


           IF  WRK-B017-VAVALC-IMOV-N  NOT EQUAL ZEROS
               MOVE '?'             TO  WRK-B017-VAVALC-IMOV-NULL
           END-IF.

           IF  WRK-B017-CSEQ-ESCRT-ONUS-N  NOT EQUAL ZEROS
               MOVE '?'             TO  WRK-B017-CSEQ-ESCRT-ONUS-NULL
           END-IF.

           IF  WRK-B017-CSEQ-PATRM-IMOV-N  NOT EQUAL ZEROS
               MOVE '?'             TO  WRK-B017-CSEQ-PATRM-IMOV-NULL
           END-IF.

           IF  WRK-B017-CCARTR-IMOV-REG-N  NOT EQUAL ZEROS
               MOVE '?'             TO  WRK-B017-CCARTR-IMOV-REG-NULL
           END-IF.

           IF  WRK-B017-CGLEBA-IMOV-HIPOT-N  NOT EQUAL ZEROS
               MOVE '?'             TO  WRK-B017-CGLEBA-IMOV-HIPOT-NUL
           END-IF.

           IF  WRK-B017-CLOTE-IMOV-HIPOT-N  NOT EQUAL ZEROS
               MOVE '?'             TO  WRK-B017-CLOTE-IMOV-HIPOT-NULL
           END-IF.


           STRING  RUECB017 (01:547)
                   WRK-B017-VAVALC-IMOV-NULL
                   RUECB017 (548:19)
                   WRK-B017-CSEQ-ESCRT-ONUS-NULL
                   RUECB017 (567:03)
                   WRK-B017-CSEQ-PATRM-IMOV-NULL
                   RUECB017 (570:15)
                   WRK-B017-CCARTR-IMOV-REG-NULL
                   RUECB017 (585:10)
                   WRK-B017-CGLEBA-IMOV-HIPOT-NUL
                   RUECB017 (595:10)
                   WRK-B017-CLOTE-IMOV-HIPOT-NULL
                   DELIMITED BY SIZE
           INTO    FD-REG-RUEC017S.


           PERFORM 7090-GRAVAR-SAIDA-RUEC017S.

      *---------------------------------------------------------------*
       2710-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       2720-CLOSE-CSR07-RUECB017       SECTION.
      *---------------------------------------------------------------*

           EXEC SQL
             CLOSE CSR07-RUECB017
           END-EXEC.

           IF  ( SQLCODE               NOT EQUAL ZEROS             )
           OR  ( SQLWARN0              EQUAL    'W'                )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'RUECB017'         TO  ERR-DBD-TAB
               MOVE 'CLOSE   '         TO  ERR-FUN-COMANDO
               MOVE SQLCODE            TO  ERR-SQL-CODE
               MOVE '2720'             TO  ERR-LOCAL
               MOVE SPACES             TO  ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       2720-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       2800-OPEN-CSR08-RUECV018        SECTION.
      *---------------------------------------------------------------*

           EXEC SQL
             OPEN CSR08-RUECV018
           END-EXEC.

           IF  ( SQLCODE               NOT EQUAL ZEROS             )
           OR  ( SQLWARN0              EQUAL    'W'                )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'RUECV018'         TO  ERR-DBD-TAB
               MOVE 'OPEN    '         TO  ERR-FUN-COMANDO
               MOVE SQLCODE            TO  ERR-SQL-CODE
               MOVE '2800'             TO  ERR-LOCAL
               MOVE SPACES             TO  ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       2800-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       2810-FETCH-CSR08-RUECV018       SECTION.
      *---------------------------------------------------------------*


           EXEC SQL
             FETCH CSR08-RUECV018
             INTO
               :RUECV018.CCONTR,
               :RUECV018.CRSUMO-MSGEM-INCON,
               :RUECV018.RMSGEM-INCON,
               :RUECV018.CINDCD-RESP-APROV
           END-EXEC.


           IF  ( SQLCODE               NOT EQUAL ZEROS AND +100    )
           OR  ( SQLWARN0              EQUAL    'W'                )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'RUECV018'         TO  ERR-DBD-TAB
               MOVE 'FETCH   '         TO  ERR-FUN-COMANDO
               MOVE SQLCODE            TO  ERR-SQL-CODE
               MOVE '2810'             TO  ERR-LOCAL
               MOVE SPACES             TO  ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO  WRK-FIM-RUEC018S
               GO                      TO  2810-99-FIM
           END-IF.

           ADD 1                       TO ACU-LIDOS-RUEC018S.

           PERFORM 7100-GRAVAR-SAIDA-RUEC018S.

      *---------------------------------------------------------------*
       2810-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       2820-CLOSE-CSR08-RUECV018       SECTION.
      *---------------------------------------------------------------*

           EXEC SQL
             CLOSE CSR08-RUECV018
           END-EXEC.

           IF  ( SQLCODE               NOT EQUAL ZEROS             )
           OR  ( SQLWARN0              EQUAL    'W'                )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'RUECV018'         TO  ERR-DBD-TAB
               MOVE 'CLOSE   '         TO  ERR-FUN-COMANDO
               MOVE SQLCODE            TO  ERR-SQL-CODE
               MOVE '2820'             TO  ERR-LOCAL
               MOVE SPACES             TO  ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       2820-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       2900-OPEN-CSR09-RUECV019        SECTION.
      *---------------------------------------------------------------*

           EXEC SQL
             OPEN CSR09-RUECV019
           END-EXEC.

           IF  ( SQLCODE               NOT EQUAL ZEROS             )
           OR  ( SQLWARN0              EQUAL    'W'                )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'RUECV019'         TO  ERR-DBD-TAB
               MOVE 'OPEN    '         TO  ERR-FUN-COMANDO
               MOVE SQLCODE            TO  ERR-SQL-CODE
               MOVE '2900'             TO  ERR-LOCAL
               MOVE SPACES             TO  ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       2900-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       2910-FETCH-CSR09-RUECV019       SECTION.
      *---------------------------------------------------------------*


           EXEC SQL
             FETCH CSR09-RUECV019
             INTO
               :RUECV019.CJUNC-DEPDC,
               :RUECV019.CCDULA,
               :RUECV019.CRSUMO-MSGEM-INCON,
               :RUECV019.RMSGEM-INCON,
               :RUECV019.CINDCD-RESP-APROV
           END-EXEC.


           IF  ( SQLCODE               NOT EQUAL ZEROS AND +100    )
           OR  ( SQLWARN0              EQUAL    'W'                )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'RUECV019'         TO  ERR-DBD-TAB
               MOVE 'FETCH   '         TO  ERR-FUN-COMANDO
               MOVE SQLCODE            TO  ERR-SQL-CODE
               MOVE '2910'             TO  ERR-LOCAL
               MOVE SPACES             TO  ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO  WRK-FIM-RUEC019S
               GO                      TO  2910-99-FIM
           END-IF.

           ADD 1                       TO ACU-LIDOS-RUEC019S.

           PERFORM 7110-GRAVAR-SAIDA-RUEC019S.

      *---------------------------------------------------------------*
       2910-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       2920-CLOSE-CSR09-RUECV019       SECTION.
      *---------------------------------------------------------------*

           EXEC SQL
             CLOSE CSR09-RUECV019
           END-EXEC.

           IF  ( SQLCODE               NOT EQUAL ZEROS             )
           OR  ( SQLWARN0              EQUAL    'W'                )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'RUECV019'         TO  ERR-DBD-TAB
               MOVE 'CLOSE   '         TO  ERR-FUN-COMANDO
               MOVE SQLCODE            TO  ERR-SQL-CODE
               MOVE '2920'             TO  ERR-LOCAL
               MOVE SPACES             TO  ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       2920-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       3000-OPEN-CSR10-RUECV022        SECTION.
      *---------------------------------------------------------------*

           EXEC SQL
             OPEN CSR10-RUECV022
           END-EXEC.

           IF  ( SQLCODE               NOT EQUAL ZEROS             )
           OR  ( SQLWARN0              EQUAL    'W'                )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'RUECV022'         TO  ERR-DBD-TAB
               MOVE 'OPEN    '         TO  ERR-FUN-COMANDO
               MOVE SQLCODE            TO  ERR-SQL-CODE
               MOVE '3000'             TO  ERR-LOCAL
               MOVE SPACES             TO  ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       3010-FETCH-CSR10-RUECV022       SECTION.
      *---------------------------------------------------------------*

           INITIALIZE WRK-RUEC022S-NULL
                     REPLACING ALPHANUMERIC DATA BY SPACES.

           INITIALIZE RUECV022.


           EXEC SQL
             FETCH CSR10-RUECV022
             INTO
               :RUECV022.CCONTR,
               :RUECV022.CPCELA-LIBRC,
               :RUECV022.RTPO-OPER-ORCAM,
               :RUECV022.CTPO-LIBRC-ORCAM,
               :RUECV022.DLIBRC-OPER-ORCAM
                 :WRK-V022-DLIBRC-OPER-ORCAM-N,
               :RUECV022.VFINCD-OPER-ORCAM,
               :RUECV022.VPCELA-REC-PPRIO,
               :RUECV022.CINDCD-LIBRC-PCELA,
               :RUECV022.DFIM-OPER-ORCAM
                 :WRK-V022-DFIM-OPER-ORCAM-N
           END-EXEC.


           IF  ( SQLCODE               NOT EQUAL ZEROS AND +100    )
           OR  ( SQLWARN0              EQUAL    'W'                )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'RUECV022'         TO  ERR-DBD-TAB
               MOVE 'FETCH   '         TO  ERR-FUN-COMANDO
               MOVE SQLCODE            TO  ERR-SQL-CODE
               MOVE '3010'             TO  ERR-LOCAL
               MOVE SPACES             TO  ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO  WRK-FIM-RUEC022S
               GO                      TO  3010-99-FIM
           END-IF.

           ADD 1                       TO ACU-LIDOS-RUEC022S.


           IF  WRK-V022-DLIBRC-OPER-ORCAM-N  NOT EQUAL ZEROS
               MOVE '?'             TO  WRK-V022-DLIBRC-OPER-ORCAM-NUL
           END-IF.

           IF  WRK-V022-DFIM-OPER-ORCAM-N   NOT EQUAL ZEROS
               MOVE '?'             TO  WRK-V022-DFIM-OPER-ORCAM-NUL
           END-IF.


           STRING  RUECV022 (01:58)
                   WRK-V022-DLIBRC-OPER-ORCAM-NUL
                   RUECV022 (59:27)
BR0318             WRK-V022-DFIM-OPER-ORCAM-NUL
                   DELIMITED BY SIZE
           INTO    FD-REG-RUEC022S.


           PERFORM 7120-GRAVAR-SAIDA-RUEC022S.

      *---------------------------------------------------------------*
       3010-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       3020-CLOSE-CSR10-RUECV022       SECTION.
      *---------------------------------------------------------------*

           EXEC SQL
             CLOSE CSR10-RUECV022
           END-EXEC.

           IF  ( SQLCODE               NOT EQUAL ZEROS             )
           OR  ( SQLWARN0              EQUAL    'W'                )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'RUECV022'         TO  ERR-DBD-TAB
               MOVE 'CLOSE   '         TO  ERR-FUN-COMANDO
               MOVE SQLCODE            TO  ERR-SQL-CODE
               MOVE '3020'             TO  ERR-LOCAL
               MOVE SPACES             TO  ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       3020-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       3100-OPEN-CSR11-RUECV023        SECTION.
      *---------------------------------------------------------------*

           EXEC SQL
             OPEN CSR11-RUECV023
           END-EXEC.

           IF  ( SQLCODE               NOT EQUAL ZEROS             )
           OR  ( SQLWARN0              EQUAL    'W'                )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'RUECV023'         TO  ERR-DBD-TAB
               MOVE 'OPEN    '         TO  ERR-FUN-COMANDO
               MOVE SQLCODE            TO  ERR-SQL-CODE
               MOVE '3100'             TO  ERR-LOCAL
               MOVE SPACES             TO  ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       3100-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       3110-FETCH-CSR11-RUECV023       SECTION.
      *---------------------------------------------------------------*


           EXEC SQL
             FETCH CSR11-RUECV023
             INTO
               :RUECV023.CCONTR,
               :RUECV023.CPCELA-LIBRC,
               :RUECV023.CISUMO-ORCAM-RURAL,
               :RUECV023.RISUMO-ORCAM-RURAL,
               :RUECV023.RQTD-UND-ISUMO,
               :RUECV023.VISUMO-ORCAM-RURAL
           END-EXEC.


           IF  ( SQLCODE               NOT EQUAL ZEROS AND +100    )
           OR  ( SQLWARN0              EQUAL    'W'                )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'RUECV023'         TO  ERR-DBD-TAB
               MOVE 'FETCH   '         TO  ERR-FUN-COMANDO
               MOVE SQLCODE            TO  ERR-SQL-CODE
               MOVE '3110'             TO  ERR-LOCAL
               MOVE SPACES             TO  ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO  WRK-FIM-RUEC023S
               GO                      TO  3110-99-FIM
           END-IF.

           ADD 1                       TO ACU-LIDOS-RUEC023S.

           PERFORM 7130-GRAVAR-SAIDA-RUEC023S.

      *---------------------------------------------------------------*
       3110-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       3120-CLOSE-CSR11-RUECV023       SECTION.
      *---------------------------------------------------------------*

           EXEC SQL
             CLOSE CSR11-RUECV023
           END-EXEC.

           IF  ( SQLCODE               NOT EQUAL ZEROS             )
           OR  ( SQLWARN0              EQUAL    'W'                )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'RUECV023'         TO  ERR-DBD-TAB
               MOVE 'CLOSE   '         TO  ERR-FUN-COMANDO
               MOVE SQLCODE            TO  ERR-SQL-CODE
               MOVE '3120'             TO  ERR-LOCAL
               MOVE SPACES             TO  ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       3120-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       3200-OPEN-CSR12-RUECB024        SECTION.
      *---------------------------------------------------------------*

           EXEC SQL
             OPEN CSR12-RUECB024
           END-EXEC.

           IF  ( SQLCODE               NOT EQUAL ZEROS             )
           OR  ( SQLWARN0              EQUAL    'W'                )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'RUECB024'         TO  ERR-DBD-TAB
               MOVE 'OPEN    '         TO  ERR-FUN-COMANDO
               MOVE SQLCODE            TO  ERR-SQL-CODE
               MOVE '3200'             TO  ERR-LOCAL
               MOVE SPACES             TO  ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       3200-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       3210-FETCH-CSR12-RUECB024       SECTION.
      *---------------------------------------------------------------*


           EXEC SQL
             FETCH CSR12-RUECB024
             INTO
               :RUECB024.CCONTR,
               :RUECB024.CLIN-OBS-ORCAM,
               :RUECB024.RLIN-OBS-ORCAM
           END-EXEC.


           IF  ( SQLCODE               NOT EQUAL ZEROS AND +100    )
           OR  ( SQLWARN0              EQUAL    'W'                )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'RUECB024'         TO  ERR-DBD-TAB
               MOVE 'FETCH   '         TO  ERR-FUN-COMANDO
               MOVE SQLCODE            TO  ERR-SQL-CODE
               MOVE '3210'             TO  ERR-LOCAL
               MOVE SPACES             TO  ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO  WRK-FIM-RUEC024S
               GO                      TO  3210-99-FIM
           END-IF.

           ADD 1                       TO ACU-LIDOS-RUEC024S.

           PERFORM 7140-GRAVAR-SAIDA-RUEC024S.

      *---------------------------------------------------------------*
       3210-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       3220-CLOSE-CSR12-RUECB024       SECTION.
      *---------------------------------------------------------------*

           EXEC SQL
             CLOSE CSR12-RUECB024
           END-EXEC.

           IF  ( SQLCODE               NOT EQUAL ZEROS             )
           OR  ( SQLWARN0              EQUAL    'W'                )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'RUECB024'         TO  ERR-DBD-TAB
               MOVE 'CLOSE   '         TO  ERR-FUN-COMANDO
               MOVE SQLCODE            TO  ERR-SQL-CODE
               MOVE '3220'             TO  ERR-LOCAL
               MOVE SPACES             TO  ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       3220-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       3300-OPEN-CSR13-RUECV025        SECTION.
      *---------------------------------------------------------------*

           EXEC SQL
             OPEN CSR13-RUECV025
           END-EXEC.

           IF  ( SQLCODE               NOT EQUAL ZEROS             )
           OR  ( SQLWARN0              EQUAL    'W'                )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'RUECV025'         TO  ERR-DBD-TAB
               MOVE 'OPEN    '         TO  ERR-FUN-COMANDO
               MOVE SQLCODE            TO  ERR-SQL-CODE
               MOVE '3300'             TO  ERR-LOCAL
               MOVE SPACES             TO  ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       3300-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       3310-FETCH-CSR13-RUECV025       SECTION.
      *---------------------------------------------------------------*

           INITIALIZE WRK-RUEC025S-NULL
                     REPLACING ALPHANUMERIC DATA BY SPACES.

           INITIALIZE RUECV025.


           MOVE SPACES                 TO  WRK-RUEC025S-N.

           EXEC SQL
             FETCH CSR13-RUECV025
             INTO
               :RUECV025.CJUNC-DEPDC,
               :RUECV025.CCDULA,
               :RUECV025.CPCELA-LIBRC,
               :RUECV025.RTPO-OPER-ORCAM,
               :RUECV025.CTPO-LIBRC-ORCAM,
               :RUECV025.DLIBRC-OPER-ORCAM
                 :WRK-V025-DLIBRC-OPER-ORCAM-N,
               :RUECV025.VFINCD-OPER-ORCAM,
               :RUECV025.VPCELA-REC-PPRIO,
               :RUECV025.CTPO-INCON-ORCAM,
               :RUECV025.CINDCD-LIBRC-PCELA
           END-EXEC.


           IF  ( SQLCODE               NOT EQUAL ZEROS AND +100    )
           OR  ( SQLWARN0              EQUAL    'W'                )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'RUECB025'         TO  ERR-DBD-TAB
               MOVE 'FETCH   '         TO  ERR-FUN-COMANDO
               MOVE SQLCODE            TO  ERR-SQL-CODE
               MOVE '3310'             TO  ERR-LOCAL
               MOVE SPACES             TO  ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO  WRK-FIM-RUEC025S
               GO                      TO  3310-99-FIM
           END-IF.

           ADD 1                       TO ACU-LIDOS-RUEC025S.

           IF  WRK-V025-DLIBRC-OPER-ORCAM-N  NOT EQUAL ZEROS
               MOVE '?'             TO  WRK-V025-DLIBRC-OPER-ORCAM-NUL
           END-IF.


           STRING  RUECV025 (01:61)
                   WRK-V025-DLIBRC-OPER-ORCAM-NUL
                   RUECV025 (62:18)
                   DELIMITED BY SIZE
           INTO    FD-REG-RUEC025S.


           PERFORM 7150-GRAVAR-SAIDA-RUEC025S.

      *---------------------------------------------------------------*
       3310-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       3320-CLOSE-CSR13-RUECV025       SECTION.
      *---------------------------------------------------------------*

           EXEC SQL
             CLOSE CSR13-RUECV025
           END-EXEC.

           IF  ( SQLCODE               NOT EQUAL ZEROS             )
           OR  ( SQLWARN0              EQUAL    'W'                )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'RUECV025'         TO  ERR-DBD-TAB
               MOVE 'CLOSE   '         TO  ERR-FUN-COMANDO
               MOVE SQLCODE            TO  ERR-SQL-CODE
               MOVE '3320'             TO  ERR-LOCAL
               MOVE SPACES             TO  ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       3320-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       3400-OPEN-CSR14-RUECV026        SECTION.
      *---------------------------------------------------------------*

           EXEC SQL
             OPEN CSR14-RUECV026
           END-EXEC.

           IF  ( SQLCODE               NOT EQUAL ZEROS             )
           OR  ( SQLWARN0              EQUAL    'W'                )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'RUECV026'         TO  ERR-DBD-TAB
               MOVE 'OPEN    '         TO  ERR-FUN-COMANDO
               MOVE SQLCODE            TO  ERR-SQL-CODE
               MOVE '3400'             TO  ERR-LOCAL
               MOVE SPACES             TO  ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       3400-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       3410-FETCH-CSR14-RUECV026       SECTION.
      *---------------------------------------------------------------*


           EXEC SQL
             FETCH CSR14-RUECV026
             INTO
               :RUECV026.CJUNC-DEPDC,
               :RUECV026.CCDULA,
               :RUECV026.CPCELA-LIBRC,
               :RUECV026.CISUMO-ORCAM-RURAL,
               :RUECV026.RISUMO-ORCAM-RURAL,
               :RUECV026.RQTD-UND-ISUMO,
               :RUECV026.VISUMO-ORCAM-RURAL
           END-EXEC.


           IF  ( SQLCODE               NOT EQUAL ZEROS AND +100    )
           OR  ( SQLWARN0              EQUAL    'W'                )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'RUECV026'         TO  ERR-DBD-TAB
               MOVE 'FETCH   '         TO  ERR-FUN-COMANDO
               MOVE SQLCODE            TO  ERR-SQL-CODE
               MOVE '3410'             TO  ERR-LOCAL
               MOVE SPACES             TO  ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO  WRK-FIM-RUEC026S
               GO                      TO  3410-99-FIM
           END-IF.

           ADD 1                       TO ACU-LIDOS-RUEC026S.

           PERFORM 7160-GRAVAR-SAIDA-RUEC026S.

      *---------------------------------------------------------------*
       3410-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       3420-CLOSE-CSR14-RUECV026       SECTION.
      *---------------------------------------------------------------*

           EXEC SQL
             CLOSE CSR14-RUECV026
           END-EXEC.

           IF  ( SQLCODE               NOT EQUAL ZEROS             )
           OR  ( SQLWARN0              EQUAL    'W'                )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'RUECV026'         TO  ERR-DBD-TAB
               MOVE 'CLOSE   '         TO  ERR-FUN-COMANDO
               MOVE SQLCODE            TO  ERR-SQL-CODE
               MOVE '3420'             TO  ERR-LOCAL
               MOVE SPACES             TO  ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       3420-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       3500-OPEN-CSR15-RUECB027        SECTION.
      *---------------------------------------------------------------*

           EXEC SQL
             OPEN CSR15-RUECB027
           END-EXEC.

           IF  ( SQLCODE               NOT EQUAL ZEROS             )
           OR  ( SQLWARN0              EQUAL    'W'                )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'RUECB027'         TO  ERR-DBD-TAB
               MOVE 'OPEN    '         TO  ERR-FUN-COMANDO
               MOVE SQLCODE            TO  ERR-SQL-CODE
               MOVE '3500'             TO  ERR-LOCAL
               MOVE SPACES             TO  ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       3500-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       3510-FETCH-CSR15-RUECB027       SECTION.
      *---------------------------------------------------------------*


           EXEC SQL
             FETCH CSR15-RUECB027
             INTO
               :RUECB027.CJUNC-DEPDC,
               :RUECB027.CCDULA,
               :RUECB027.CLIN-OBS-ORCAM,
               :RUECB027.RLIN-OBS-ORCAM
           END-EXEC.


           IF  ( SQLCODE               NOT EQUAL ZEROS AND +100    )
           OR  ( SQLWARN0              EQUAL    'W'                )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'RUECB027'         TO  ERR-DBD-TAB
               MOVE 'FETCH   '         TO  ERR-FUN-COMANDO
               MOVE SQLCODE            TO  ERR-SQL-CODE
               MOVE '3510'             TO  ERR-LOCAL
               MOVE SPACES             TO  ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO  WRK-FIM-RUEC027S
               GO                      TO  3510-99-FIM
           END-IF.

           ADD 1                       TO ACU-LIDOS-RUEC027S.

           PERFORM 7170-GRAVAR-SAIDA-RUEC027S.

      *---------------------------------------------------------------*
       3510-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       3520-CLOSE-CSR15-RUECB027       SECTION.
      *---------------------------------------------------------------*

           EXEC SQL
             CLOSE CSR15-RUECB027
           END-EXEC.

           IF  ( SQLCODE               NOT EQUAL ZEROS             )
           OR  ( SQLWARN0              EQUAL    'W'                )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'RUECB027'         TO  ERR-DBD-TAB
               MOVE 'CLOSE   '         TO  ERR-FUN-COMANDO
               MOVE SQLCODE            TO  ERR-SQL-CODE
               MOVE '3520'             TO  ERR-LOCAL
               MOVE SPACES             TO  ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       3520-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       3600-OPEN-CSR16-RUECB028        SECTION.
      *---------------------------------------------------------------*

           EXEC SQL
             OPEN CSR16-RUECB028
           END-EXEC.

           IF  ( SQLCODE               NOT EQUAL ZEROS             )
           OR  ( SQLWARN0              EQUAL    'W'                )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'RUECB028'         TO  ERR-DBD-TAB
               MOVE 'OPEN    '         TO  ERR-FUN-COMANDO
               MOVE SQLCODE            TO  ERR-SQL-CODE
               MOVE '3600'             TO  ERR-LOCAL
               MOVE SPACES             TO  ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       3600-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       3610-FETCH-CSR16-RUECB028       SECTION.
      *---------------------------------------------------------------*

           EXEC SQL
             FETCH CSR16-RUECB028
             INTO
               :RUECB028.CCONTR,
               :RUECB028.CSEQ-ARZEM-RURAL,
               :RUECB028.IARZEM-PRODT-RURAL,
               :RUECB028.IPPRIE-ARZEM-PRODT,
               :RUECB028.RLOC-ARZEM-PRODT,
               :RUECB028.CREG-CONAB-ARZEM,
               :RUECB028.CREG-CIBRAZEM,
               :RUECB028.CCNPJ-EMPR,
               :RUECB028.CFLIAL-CNPJ-EMPR,
               :RUECB028.CCTRL-CPF-CNPJ,
               :RUECB028.QEXPDI-ARZEM-ENSAC,
               :RUECB028.QEXPDI-ARZEM-GRANL,
               :RUECB028.CMUN-RURAL
           END-EXEC.


           IF  ( SQLCODE               NOT EQUAL ZEROS AND +100    )
           OR  ( SQLWARN0              EQUAL    'W'                )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'RUECB028'         TO  ERR-DBD-TAB
               MOVE 'FETCH   '         TO  ERR-FUN-COMANDO
               MOVE SQLCODE            TO  ERR-SQL-CODE
               MOVE '3610'             TO  ERR-LOCAL
               MOVE SPACES             TO  ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO  WRK-FIM-RUEC028S
               GO                      TO  3610-99-FIM
           END-IF.

           ADD 1                       TO ACU-LIDOS-RUEC028S.

           PERFORM 7180-GRAVAR-SAIDA-RUEC028S.

      *---------------------------------------------------------------*
       3610-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       3620-CLOSE-CSR16-RUECB028       SECTION.
      *---------------------------------------------------------------*

           EXEC SQL
             CLOSE CSR16-RUECB028
           END-EXEC.

           IF  ( SQLCODE               NOT EQUAL ZEROS             )
           OR  ( SQLWARN0              EQUAL    'W'                )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'RUECB028'         TO  ERR-DBD-TAB
               MOVE 'CLOSE   '         TO  ERR-FUN-COMANDO
               MOVE SQLCODE            TO  ERR-SQL-CODE
               MOVE '3620'             TO  ERR-LOCAL
               MOVE SPACES             TO  ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       3620-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       3700-ACESSAR-RUECB033           SECTION.
      *---------------------------------------------------------------*

           EXEC SQL
           SELECT
             CJUNC_DEPDC
           , CCDULA
           , DIMPRE
           , CINDCD_LOC_FIS
           INTO
             :RUECB033.CJUNC-DEPDC,
             :RUECB033.CCDULA,
             :RUECB033.DIMPRE,
             :RUECB033.CINDCD-LOC-FIS
           FROM  DB2PRD.CTRL_HIST_IMPRE
           WHERE   CJUNC_DEPDC = :WRK-B007-CJUNC-DEPDC
           AND     CCDULA      = :WRK-B007-CCDULA
           END-EXEC.


           IF  ( SQLCODE               NOT EQUAL ZEROS AND +100    )
           OR  ( SQLWARN0              EQUAL    'W'                )
             MOVE 'DB2'              TO  ERR-TIPO-ACESSO
             MOVE 'RUECB033'         TO  ERR-DBD-TAB
             MOVE 'FETCH   '         TO  ERR-FUN-COMANDO
             MOVE SQLCODE            TO  ERR-SQL-CODE
             MOVE '3700'             TO  ERR-LOCAL
             MOVE SPACES             TO  ERR-SEGM
             PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL +100
               GO                      TO  3700-99-FIM
           END-IF.

           ADD 1                       TO ACU-LIDOS-RUEC033S.

           PERFORM 7190-GRAVAR-SAIDA-RUEC033S.


      *---------------------------------------------------------------*
       3700-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       3800-OPEN-CSR18-RUECB034        SECTION.
      *---------------------------------------------------------------*

           EXEC SQL
             OPEN CSR18-RUECB034
           END-EXEC.

           IF  ( SQLCODE               NOT EQUAL ZEROS             )
           OR  ( SQLWARN0              EQUAL    'W'                )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'RUECB034'         TO  ERR-DBD-TAB
               MOVE 'OPEN    '         TO  ERR-FUN-COMANDO
               MOVE SQLCODE            TO  ERR-SQL-CODE
               MOVE '3800'             TO  ERR-LOCAL
               MOVE SPACES             TO  ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       3800-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       3810-FETCH-CSR18-RUECB034       SECTION.
      *---------------------------------------------------------------*


           EXEC SQL
             FETCH CSR18-RUECB034
             INTO
               :RUECB034.CJUNC-DEPDC,
               :RUECB034.CCDULA,
               :RUECB034.CLIN-FIS-TEXTO,
               :RUECB034.RLIN-MAX,
               :RUECB034.CPAG
           END-EXEC.


           IF  ( SQLCODE               NOT EQUAL ZEROS AND +100    )
           OR  ( SQLWARN0              EQUAL    'W'                )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'RUECB034'         TO  ERR-DBD-TAB
               MOVE 'FETCH   '         TO  ERR-FUN-COMANDO
               MOVE SQLCODE            TO  ERR-SQL-CODE
               MOVE '3810'             TO  ERR-LOCAL
               MOVE SPACES             TO  ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO  WRK-FIM-RUEC034S
               GO                      TO  3810-99-FIM
           END-IF.

           ADD 1                       TO ACU-LIDOS-RUEC034S.

           PERFORM 7200-GRAVAR-SAIDA-RUEC034S.

      *---------------------------------------------------------------*
       3810-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       3820-CLOSE-CSR18-RUECB034       SECTION.
      *---------------------------------------------------------------*

           EXEC SQL
             CLOSE CSR18-RUECB034
           END-EXEC.

           IF  ( SQLCODE               NOT EQUAL ZEROS             )
           OR  ( SQLWARN0              EQUAL    'W'                )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'RUECB034'         TO  ERR-DBD-TAB
               MOVE 'CLOSE   '         TO  ERR-FUN-COMANDO
               MOVE SQLCODE            TO  ERR-SQL-CODE
               MOVE '3820'             TO  ERR-LOCAL
               MOVE SPACES             TO  ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       3820-99-FIM.                    EXIT.
      *---------------------------------------------------------------*


      *---------------------------------------------------------------*
       3900-OPEN-CSR19-RUECB035        SECTION.
      *---------------------------------------------------------------*

           EXEC SQL
             OPEN CSR19-RUECB035
           END-EXEC.

           IF  ( SQLCODE               NOT EQUAL ZEROS             )
           OR  ( SQLWARN0              EQUAL    'W'                )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'RUECB035'         TO  ERR-DBD-TAB
               MOVE 'OPEN    '         TO  ERR-FUN-COMANDO
               MOVE SQLCODE            TO  ERR-SQL-CODE
               MOVE '3900'             TO  ERR-LOCAL
               MOVE SPACES             TO  ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       3900-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       3910-FETCH-CSR19-RUECB035       SECTION.
      *---------------------------------------------------------------*

           INITIALIZE WRK-RUEC035S-NULL
                     REPLACING ALPHANUMERIC DATA BY SPACES.

           INITIALIZE RUECB035.


           EXEC SQL
             FETCH CSR19-RUECB035
             INTO
               :RUECB035.CJUNC-DEPDC,
               :RUECB035.CCDULA,
               :RUECB035.CSEQ-HIPOT-RURAL,
               :RUECB035.IPPRIE-IMOV,
               :RUECB035.RLOC-IMOV,
               :RUECB035.MAREA-IMOV,
               :RUECB035.CUND-MEDD,
               :RUECB035.DAQUIS-IMOV,
               :RUECB035.CFL-LIVRO-REG-IMOV,
               :RUECB035.CLIVRO-REG-IMOV,
               :RUECB035.ICARTR,
               :RUECB035.ICOMAR,
               :RUECB035.IDOCTO-POSSE-IMOV,
               :RUECB035.VAVALC-IMOV,
               :RUECB035.CMATR-IMOV-RURAL,
               :RUECB035.CGRAU-PNHOR,
               :RUECB035.CSEQ-ESCRT-ONUS
                 :WRK-B035-CSEQ-ESCRT-ONUS-N,
               :RUECB035.CSEQ-PATRM-IMOV
                 :WRK-B035-CSEQ-PATRM-IMOV-N,
               :RUECB035.CMUN-RURAL,
               :RUECB035.CCARTR-IMOV-REG
                 :WRK-B035-CCARTR-IMOV-REG-N,
               :RUECB035.CGLEBA-IMOV-HIPOT
                 :WRK-B035-CGLEBA-IMOV-HIPOT-N,
               :RUECB035.CLOTE-IMOV-HIPOT
                 :WRK-B035-CLOTE-IMOV-HIPOT-N
           END-EXEC.


           IF  ( SQLCODE               NOT EQUAL ZEROS AND +100    )
           OR  ( SQLWARN0              EQUAL    'W'                )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'RUECB035'         TO  ERR-DBD-TAB
               MOVE 'FETCH   '         TO  ERR-FUN-COMANDO
               MOVE SQLCODE            TO  ERR-SQL-CODE
               MOVE '3910'             TO  ERR-LOCAL
               MOVE SPACES             TO  ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO  WRK-FIM-RUEC035S
               GO                      TO  3910-99-FIM
           END-IF.

           ADD 1                       TO ACU-LIDOS-RUEC035S.


           IF  WRK-B035-CSEQ-ESCRT-ONUS-N  NOT EQUAL ZEROS
               MOVE '?'              TO  WRK-B035-CSEQ-ESCRT-ONUS-NULL
           END-IF.

           IF  WRK-B035-CSEQ-PATRM-IMOV-N  NOT EQUAL ZEROS
               MOVE '?'              TO  WRK-B035-CSEQ-PATRM-IMOV-NULL
           END-IF.

           IF  WRK-B035-CCARTR-IMOV-REG-N  NOT EQUAL ZEROS
               MOVE '?'              TO  WRK-B035-CCARTR-IMOV-REG-NULL
           END-IF.

           IF  WRK-B035-CGLEBA-IMOV-HIPOT-N  NOT EQUAL ZEROS
               MOVE '?'              TO  WRK-B035-CGLEBA-IMOV-HIPOT-NUL
           END-IF.

           IF  WRK-B035-CLOTE-IMOV-HIPOT-N  NOT EQUAL ZEROS
               MOVE '?'              TO  WRK-B035-CLOTE-IMOV-HIPOT-NULL
           END-IF.


           STRING  RUECB035 (01:569)
                   WRK-B035-CSEQ-ESCRT-ONUS-NULL
                   RUECB035 (570:03)
                   WRK-B035-CSEQ-PATRM-IMOV-NULL
                   RUECB035 (573:15)
                   WRK-B035-CCARTR-IMOV-REG-NULL
                   RUECB035 (588:10)
                   WRK-B035-CGLEBA-IMOV-HIPOT-NUL
                   RUECB035 (598:10)
                   WRK-B035-CLOTE-IMOV-HIPOT-NULL
                   DELIMITED BY SIZE
           INTO    FD-REG-RUEC035S.


           PERFORM 7210-GRAVAR-SAIDA-RUEC035S.

      *---------------------------------------------------------------*
       3910-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       3920-CLOSE-CSR19-RUECB035       SECTION.
      *---------------------------------------------------------------*

           EXEC SQL
             CLOSE CSR19-RUECB035
           END-EXEC.

           IF  ( SQLCODE               NOT EQUAL ZEROS             )
           OR  ( SQLWARN0              EQUAL    'W'                )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'RUECB035'         TO  ERR-DBD-TAB
               MOVE 'CLOSE   '         TO  ERR-FUN-COMANDO
               MOVE SQLCODE            TO  ERR-SQL-CODE
               MOVE '3920'             TO  ERR-LOCAL
               MOVE SPACES             TO  ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       3920-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       4000-OPEN-CSR20-RUECB036        SECTION.
      *---------------------------------------------------------------*

           EXEC SQL
             OPEN CSR20-RUECB036
           END-EXEC.

           IF  ( SQLCODE               NOT EQUAL ZEROS             )
           OR  ( SQLWARN0              EQUAL    'W'                )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'RUECB036'         TO  ERR-DBD-TAB
               MOVE 'OPEN    '         TO  ERR-FUN-COMANDO
               MOVE SQLCODE            TO  ERR-SQL-CODE
               MOVE '4000'             TO  ERR-LOCAL
               MOVE SPACES             TO  ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       4000-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       4010-FETCH-CSR20-RUECB036       SECTION.
      *---------------------------------------------------------------*


           EXEC SQL
             FETCH CSR20-RUECB036
             INTO
               :RUECB036.CJUNC-DEPDC,
               :RUECB036.CCDULA,
               :RUECB036.CSEQ-HIPOT-RURAL,
               :RUECB036.CLIN-FIS-TEXTO,
               :RUECB036.RLIN-MAX
           END-EXEC.


           IF  ( SQLCODE               NOT EQUAL ZEROS AND +100    )
           OR  ( SQLWARN0              EQUAL    'W'                )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'RUECB036'         TO  ERR-DBD-TAB
               MOVE 'FETCH   '         TO  ERR-FUN-COMANDO
               MOVE SQLCODE            TO  ERR-SQL-CODE
               MOVE '4010'             TO  ERR-LOCAL
               MOVE SPACES             TO  ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO  WRK-FIM-RUEC036S
               GO                      TO  4010-99-FIM
           END-IF.

           ADD 1                       TO ACU-LIDOS-RUEC036S.

           PERFORM 7220-GRAVAR-SAIDA-RUEC036S.

      *---------------------------------------------------------------*
       4010-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       4020-CLOSE-CSR20-RUECB036       SECTION.
      *---------------------------------------------------------------*

           EXEC SQL
             CLOSE CSR20-RUECB036
           END-EXEC.

           IF  ( SQLCODE               NOT EQUAL ZEROS             )
           OR  ( SQLWARN0              EQUAL    'W'                )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'RUECB036'         TO  ERR-DBD-TAB
               MOVE 'CLOSE   '         TO  ERR-FUN-COMANDO
               MOVE SQLCODE            TO  ERR-SQL-CODE
               MOVE '4020'             TO  ERR-LOCAL
               MOVE SPACES             TO  ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       4020-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       4100-OPEN-CSR21-RUECB038        SECTION.
      *---------------------------------------------------------------*

           EXEC SQL
             OPEN CSR21-RUECB038
           END-EXEC.

           IF  ( SQLCODE               NOT EQUAL ZEROS             )
           OR  ( SQLWARN0              EQUAL    'W'                )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'RUECB038'         TO  ERR-DBD-TAB
               MOVE 'OPEN    '         TO  ERR-FUN-COMANDO
               MOVE SQLCODE            TO  ERR-SQL-CODE
               MOVE '4100'             TO  ERR-LOCAL
               MOVE SPACES             TO  ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       4100-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       4110-FETCH-CSR21-RUECB038       SECTION.
      *---------------------------------------------------------------*


           EXEC SQL
             FETCH CSR21-RUECB038
             INTO
               :RUECB038.CCONTR,
               :RUECB038.CSEQ-HIPOT-RURAL,
               :RUECB038.CLIN-FIS-TEXTO,
               :RUECB038.RLIN-MAX
           END-EXEC.


           IF  ( SQLCODE               NOT EQUAL ZEROS AND +100    )
           OR  ( SQLWARN0              EQUAL    'W'                )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'RUECB038'         TO  ERR-DBD-TAB
               MOVE 'FETCH   '         TO  ERR-FUN-COMANDO
               MOVE SQLCODE            TO  ERR-SQL-CODE
               MOVE '4110'             TO  ERR-LOCAL
               MOVE SPACES             TO  ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO  WRK-FIM-RUEC038S
               GO                      TO  4110-99-FIM
           END-IF.

           ADD 1                       TO ACU-LIDOS-RUEC038S.

           PERFORM 7230-GRAVAR-SAIDA-RUEC038S.

      *---------------------------------------------------------------*
       4110-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       4120-CLOSE-CSR21-RUECB038       SECTION.
      *---------------------------------------------------------------*

           EXEC SQL
             CLOSE CSR21-RUECB038
           END-EXEC.

           IF  ( SQLCODE               NOT EQUAL ZEROS             )
           OR  ( SQLWARN0              EQUAL    'W'                )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'RUECB038'         TO  ERR-DBD-TAB
               MOVE 'CLOSE   '         TO  ERR-FUN-COMANDO
               MOVE SQLCODE            TO  ERR-SQL-CODE
               MOVE '4120'             TO  ERR-LOCAL
               MOVE SPACES             TO  ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       4120-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       4200-OPEN-CSR22-RUECB040        SECTION.
      *---------------------------------------------------------------*

           EXEC SQL
             OPEN CSR22-RUECB040
           END-EXEC.

           IF  ( SQLCODE               NOT EQUAL ZEROS             )
           OR  ( SQLWARN0              EQUAL    'W'                )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'RUECB040'         TO  ERR-DBD-TAB
               MOVE 'OPEN    '         TO  ERR-FUN-COMANDO
               MOVE SQLCODE            TO  ERR-SQL-CODE
               MOVE '4200'             TO  ERR-LOCAL
               MOVE SPACES             TO  ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       4200-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       4210-FETCH-CSR22-RUECB040       SECTION.
      *---------------------------------------------------------------*

           INITIALIZE WRK-RUEC040S-NULL
                     REPLACING ALPHANUMERIC DATA BY SPACES.

           INITIALIZE RUECB040.


           EXEC SQL
             FETCH CSR22-RUECB040
             INTO
               :RUECB040.CCONTR,
               :RUECB040.CSEQ-MATR-RURAL,
               :RUECB040.CINDCD-AREA-PRINC,
               :RUECB040.CMATR-IMOV-RURAL,
               :RUECB040.IIMOV-RURAL,
               :RUECB040.MAREA-IMOV-RURAL,
               :RUECB040.CMUN-RURAL,
               :RUECB040.CSEQ-IMOV-RURAL,
               :RUECB040.CID-GLEBA
                    :WRK-B040-CID-N,
               :RUECB040.MLATTD-PTO-CNTRL
                    :WRK-B040-MLATTD-N,
               :RUECB040.MLOGTD-PTO-CNTRL
                    :WRK-B040-MLOGTD-N
           END-EXEC.


           IF  ( SQLCODE               NOT EQUAL ZEROS AND +100    )
           OR  ( SQLWARN0              EQUAL    'W'                )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'RUECB040'         TO  ERR-DBD-TAB
               MOVE 'FETCH   '         TO  ERR-FUN-COMANDO
               MOVE SQLCODE            TO  ERR-SQL-CODE
               MOVE '4210'             TO  ERR-LOCAL
               MOVE SPACES             TO  ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO  WRK-FIM-RUEC040S
               GO                      TO  4210-99-FIM
           END-IF.

           IF  SQLCODE      EQUAL ZEROS
               PERFORM 4211-VER-NULOS-B040
           END-IF.

           ADD 1                       TO ACU-LIDOS-RUEC040S.

           PERFORM 7240-GRAVAR-SAIDA-RUEC040S.

      *---------------------------------------------------------------*
       4210-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       4211-VER-NULOS-B040        SECTION.
      *---------------------------------------------------------------*

           IF WRK-B040-CID-N              NOT EQUAL  ZEROS
              MOVE '?'             TO  WRK-B040-CID-NULL
           END-IF.

           IF WRK-B040-MLATTD-N           NOT EQUAL  ZEROS
              MOVE '?'             TO  WRK-B040-MLATTD-NULL
           END-IF.

           IF WRK-B040-MLOGTD-N           NOT EQUAL  ZEROS
              MOVE  '?'            TO WRK-B040-MLOGTD-NULL
           END-IF.


           STRING  RUECB040 (01:79)
                   WRK-B040-CID-NULL
                   RUECB040 (80:05)
                   WRK-B040-MLATTD-NULL
                   RUECB040 (85:05)
                   WRK-B040-MLOGTD-NULL
                   DELIMITED BY SIZE
           INTO    FD-REG-RUEC040S.

      *---------------------------------------------------------------*
       4211-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       4220-CLOSE-CSR22-RUECB040       SECTION.
      *---------------------------------------------------------------*

           EXEC SQL
             CLOSE CSR22-RUECB040
           END-EXEC.

           IF  ( SQLCODE               NOT EQUAL ZEROS             )
           OR  ( SQLWARN0              EQUAL    'W'                )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'RUECB040'         TO  ERR-DBD-TAB
               MOVE 'CLOSE   '         TO  ERR-FUN-COMANDO
               MOVE SQLCODE            TO  ERR-SQL-CODE
               MOVE '4220'             TO  ERR-LOCAL
               MOVE SPACES             TO  ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       4220-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       4300-OPEN-CSR23-RUECB041        SECTION.
      *---------------------------------------------------------------*

           EXEC SQL
             OPEN CSR23-RUECB041
           END-EXEC.

           IF  ( SQLCODE               NOT EQUAL ZEROS             )
           OR  ( SQLWARN0              EQUAL    'W'                )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'RUECB041'         TO  ERR-DBD-TAB
               MOVE 'OPEN    '         TO  ERR-FUN-COMANDO
               MOVE SQLCODE            TO  ERR-SQL-CODE
               MOVE '4300'             TO  ERR-LOCAL
               MOVE SPACES             TO  ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       4300-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       4310-FETCH-CSR23-RUECB041       SECTION.
      *---------------------------------------------------------------*


           INITIALIZE WRK-RUEC041S-NULL
                     REPLACING ALPHANUMERIC DATA BY SPACES.

           INITIALIZE RUECB041.


           EXEC SQL
             FETCH CSR23-RUECB041
             INTO
               :RUECB041.CJUNC-DEPDC,
               :RUECB041.CCDULA,
               :RUECB041.CSEQ-MATR-RURAL,
               :RUECB041.CINDCD-AREA-PRINC,
               :RUECB041.CMATR-IMOV-RURAL,
               :RUECB041.IIMOV-RURAL,
               :RUECB041.MAREA-IMOV-RURAL,
               :RUECB041.CSEQ-IMOV-RURAL,
               :RUECB041.CMUN-RURAL,
               :RUECB041.CID-GLEBA
                    :WRK-B041-CID-N,
               :RUECB041.MLATTD-PTO-CNTRL
                    :WRK-B041-MLATTD-N,
               :RUECB041.MLOGTD-PTO-CNTRL
                    :WRK-B041-MLOGTD-N
           END-EXEC.


           IF  ( SQLCODE               NOT EQUAL ZEROS AND +100    )
           OR  ( SQLWARN0              EQUAL    'W'                )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'RUECB041'         TO  ERR-DBD-TAB
               MOVE 'FETCH   '         TO  ERR-FUN-COMANDO
               MOVE SQLCODE            TO  ERR-SQL-CODE
               MOVE '4310'             TO  ERR-LOCAL
               MOVE SPACES             TO  ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO  WRK-FIM-RUEC041S
               GO                      TO  4310-99-FIM
           END-IF.

           IF  SQLCODE      EQUAL ZEROS
               PERFORM 4311-VER-NULOS-B041
           END-IF.

           ADD 1                       TO ACU-LIDOS-RUEC041S.

           PERFORM 7250-GRAVAR-SAIDA-RUEC041S.

      *---------------------------------------------------------------*
       4310-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       4311-VER-NULOS-B041        SECTION.
      *---------------------------------------------------------------*

           IF WRK-B041-CID-N             NOT EQUAL   ZEROS
              MOVE '?'     TO WRK-B041-CID-NULL
           END-IF.

           IF WRK-B041-MLATTD-N          NOT EQUAL   ZEROS
              MOVE '?'    TO  WRK-B041-MLATTD-NULL
           END-IF.

           IF WRK-B041-MLOGTD-N          NOT EQUAL   ZEROS
              MOVE '?'    TO  WRK-B041-MLOGTD-NULL
           END-IF.


           STRING  RUECB041 (01:82)
                   WRK-B041-CID-NULL
                   RUECB041 (83:05)
                   WRK-B041-MLATTD-NULL
                   RUECB041 (88:05)
                   WRK-B041-MLOGTD-NULL
                   DELIMITED BY SIZE
           INTO    FD-REG-RUEC041S.

      *---------------------------------------------------------------*
       4311-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       4320-CLOSE-CSR23-RUECB041       SECTION.
      *---------------------------------------------------------------*

           EXEC SQL
             CLOSE CSR23-RUECB041
           END-EXEC.

           IF  ( SQLCODE               NOT EQUAL ZEROS             )
           OR  ( SQLWARN0              EQUAL    'W'                )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'RUECB041'         TO  ERR-DBD-TAB
               MOVE 'CLOSE   '         TO  ERR-FUN-COMANDO
               MOVE SQLCODE            TO  ERR-SQL-CODE
               MOVE '4320'             TO  ERR-LOCAL
               MOVE SPACES             TO  ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       4320-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       4400-ACESSAR-RUECB044           SECTION.
      *---------------------------------------------------------------*

           INITIALIZE WRK-RUEC044S-NULL
                     REPLACING ALPHANUMERIC DATA BY SPACES.

           INITIALIZE RUECB044.


           EXEC SQL
           SELECT
             CJUNC_DEPDC
           , CCDULA
           , CCONTR
           , RCTDAO_INSS
           , DEXPDI_CTDAO_INSS
           , CTPO_CTDAO_INSS
           , RCTDAO_RECTA_FEDRL
           , DEXPDI_CTDAO_RECTA
           , RAG_RECTA_DSTRI
           , CTPO_CTDAO_RECTA
           , DVCTO_CTDAO_INSS
           , RCEI
           , DVCTO_CTDAO_RECTA
           , HEMIS_CTDAO_RECTA
           , RCERTF_FGTS
           , DVCTO_CERTF_FGTS
           , DEMIS_CERTF_FGTS
           , HEMIS_CERTF_FGTS
           , DEMIS_LICEN_AMBTL
           , CINDCD_LICEN_AMBTL
           , RCOMPV_RAIS
           , DEMIS_COMPV_RAIS
           , DINCL_CTDAO_BNDES
           , CFUNC_BDSCO_INCL
           , HULT_ATULZ
           , CFUNC_ULT_ATULZ
           , CLICEN_AMBTL
           , CTPO_LICEN_AMBTL
           , DFNAL_LICEN_AMBTL
           , IORG_CONCS_LICEN
           , CSGL_UF_CONCS
           , RFNALD_CONCS_LICEN
           , CINDCD_PORTE_BNEFC
           INTO
             :RUECB044.CJUNC-DEPDC,
             :RUECB044.CCDULA,
             :RUECB044.CCONTR,
             :RUECB044.RCTDAO-INSS,
             :RUECB044.DEXPDI-CTDAO-INSS,
             :RUECB044.CTPO-CTDAO-INSS,
             :RUECB044.RCTDAO-RECTA-FEDRL,
             :RUECB044.DEXPDI-CTDAO-RECTA,
             :RUECB044.RAG-RECTA-DSTRI,
             :RUECB044.CTPO-CTDAO-RECTA,
             :RUECB044.DVCTO-CTDAO-INSS,
             :RUECB044.RCEI,
             :RUECB044.DVCTO-CTDAO-RECTA,
             :RUECB044.HEMIS-CTDAO-RECTA,
             :RUECB044.RCERTF-FGTS,
             :RUECB044.DVCTO-CERTF-FGTS,
             :RUECB044.DEMIS-CERTF-FGTS,
             :RUECB044.HEMIS-CERTF-FGTS,
             :RUECB044.DEMIS-LICEN-AMBTL,
             :RUECB044.CINDCD-LICEN-AMBTL,
             :RUECB044.RCOMPV-RAIS,
             :RUECB044.DEMIS-COMPV-RAIS,
             :RUECB044.DINCL-CTDAO-BNDES,
             :RUECB044.CFUNC-BDSCO-INCL,
             :RUECB044.HULT-ATULZ
               :WRK-B044-HULT-ATULZ-N,
             :RUECB044.CFUNC-ULT-ATULZ
               :WRK-B044-CFUNC-ULT-ATULZ-N,
             :RUECB044.CLICEN-AMBTL,
             :RUECB044.CTPO-LICEN-AMBTL,
             :RUECB044.DFNAL-LICEN-AMBTL,
             :RUECB044.IORG-CONCS-LICEN,
             :RUECB044.CSGL-UF-CONCS,
             :RUECB044.RFNALD-CONCS-LICEN,
             :RUECB044.CINDCD-PORTE-BNEFC
           FROM  DB2PRD.CTDAO_BNDES
           WHERE   CJUNC_DEPDC = :WRK-B007-CJUNC-DEPDC
           AND     CCDULA      = :WRK-B007-CCDULA
           END-EXEC.


           IF  ( SQLCODE               NOT EQUAL ZEROS AND +100    )
           OR  ( SQLWARN0              EQUAL    'W'                )
             MOVE 'DB2'              TO  ERR-TIPO-ACESSO
             MOVE 'RUECB044'         TO  ERR-DBD-TAB
             MOVE 'FETCH   '         TO  ERR-FUN-COMANDO
             MOVE SQLCODE            TO  ERR-SQL-CODE
             MOVE '4400'             TO  ERR-LOCAL
             MOVE SPACES             TO  ERR-SEGM
             PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL +100
               GO                      TO  4400-99-FIM
           END-IF.


           ADD 1                       TO ACU-LIDOS-RUEC044S.


           IF  WRK-B044-HULT-ATULZ-N  NOT EQUAL ZEROS
               MOVE '?'              TO  WRK-B044-HULT-ATULZ-NULL
           END-IF.

           IF  WRK-B044-CFUNC-ULT-ATULZ-N  NOT EQUAL ZEROS
               MOVE '?'              TO  WRK-B044-CFUNC-ULT-ATULZ-NULL
           END-IF.


           STRING  RUECB044 (01:303)
                   WRK-B044-HULT-ATULZ-NULL
                   RUECB044 (304:05)
                   WRK-B044-CFUNC-ULT-ATULZ-NULL
                   RUECB044 (309:180)
                   DELIMITED BY SIZE
           INTO    FD-REG-RUEC044S.


           PERFORM 7260-GRAVAR-SAIDA-RUEC044S.


      *---------------------------------------------------------------*
       4400-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       4500-OPEN-CSR25-RUECB052        SECTION.
      *---------------------------------------------------------------*

           EXEC SQL
             OPEN CSR25-RUECB052
           END-EXEC.

           IF  ( SQLCODE               NOT EQUAL ZEROS             )
           OR  ( SQLWARN0              EQUAL    'W'                )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'RUECB052'         TO  ERR-DBD-TAB
               MOVE 'OPEN    '         TO  ERR-FUN-COMANDO
               MOVE SQLCODE            TO  ERR-SQL-CODE
               MOVE '4500'             TO  ERR-LOCAL
               MOVE SPACES             TO  ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       4500-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       4510-FETCH-CSR25-RUECB052       SECTION.
      *---------------------------------------------------------------*

           INITIALIZE WRK-RUEC052S-NULL
                     REPLACING ALPHANUMERIC DATA BY SPACES.

           INITIALIZE RUECB052.


           MOVE SPACES                 TO  WRK-RUEC052S-N.

           EXEC SQL
             FETCH CSR25-RUECB052
             INTO
               :RUECB052.CCONTR,
               :RUECB052.CSEQ-HIST-IMOV,
               :RUECB052.RIMOV-RURAL,
               :RUECB052.RTPO-EXPLO-IMOV,
               :RUECB052.PIMOV-CLI,
               :RUECB052.ELOGDR,
               :RUECB052.ENRO-LOGDR,
               :RUECB052.ECOMPL-LOGDR,
               :RUECB052.EBAIRO-LOGDR,
               :RUECB052.CMUN-RURAL
                 :WRK-B052-CMUN-RURAL-N,
               :RUECB052.CCEP
                 :WRK-B052-CCEP-N,
               :RUECB052.CCEP-COMPL
                 : WRK-B052-CCEP-COMPL-N,
               :RUECB052.MAREA-CULTR-AGRIC,
               :RUECB052.MAREA-PASTA,
               :RUECB052.MDMAIS-AREA,
               :RUECB052.RROTRO-ACSSO,
               :RUECB052.HULT-ATULZ-EFETU,
               :RUECB052.CFUNC-BDSCO,
               :RUECB052.RTPO-IMOV-ARREN
                 :WRK-B052-RTPO-IMOV-ARREN-N,
               :RUECB052.DAQUIS-IMOV,
               :RUECB052.RTPO-DOCTO
                 :WRK-B052-RTPO-DOCTO-N,
               :RUECB052.CLIVRO-REG-IMOV,
               :RUECB052.DREG-IMOV,
               :RUECB052.CINDCD-IMOV-IMPNH,
               :RUECB052.VATUAL-IMOV-RURAL,
               :RUECB052.CCARTR-IMOV-REG
                 :WRK-B052-CCARTR-IMOV-REG-N,
               :RUECB052.CMATR-IMOV-REG,
               :RUECB052.NFL-IMOV-REG,
               :RUECB052.IRGIAO-JUDIC-IMOV,
               :RUECB052.ICARTR-CONTR-RURAL,
               :RUECB052.CGLEBA-IMOV-HIPOT,
               :RUECB052.CLOTE-IMOV-HIPOT
           END-EXEC.


           IF  ( SQLCODE               NOT EQUAL ZEROS AND +100    )
           OR  ( SQLWARN0              EQUAL    'W'                )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'RUECB052'         TO  ERR-DBD-TAB
               MOVE 'FETCH   '         TO  ERR-FUN-COMANDO
               MOVE SQLCODE            TO  ERR-SQL-CODE
               MOVE '4510'             TO  ERR-LOCAL
               MOVE SPACES             TO  ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO  WRK-FIM-RUEC052S
               GO                      TO  4510-99-FIM
           END-IF.

           ADD 1                       TO ACU-LIDOS-RUEC052S.

           IF  WRK-B052-CMUN-RURAL-N  NOT EQUAL ZEROS
               MOVE '?'               TO  WRK-B052-CMUN-RURAL-NULL
           END-IF.

           IF  WRK-B052-CCEP-N        NOT EQUAL ZEROS
               MOVE '?'               TO  WRK-B052-CCEP-NULL
           END-IF.

           IF  WRK-B052-CCEP-COMPL-N  NOT EQUAL ZEROS
               MOVE '?'               TO  WRK-B052-CCEP-COMPL-NULL
           END-IF.

           IF  WRK-B052-RTPO-IMOV-ARREN-N   NOT EQUAL ZEROS
               MOVE '?'            TO  WRK-B052-RTPO-IMOV-ARREN-NULL
           END-IF.

           IF  WRK-B052-RTPO-DOCTO-N  NOT EQUAL ZEROS
               MOVE '?'               TO  WRK-B052-RTPO-DOCTO-NULL
           END-IF.

           IF  WRK-B052-CCARTR-IMOV-REG-N  NOT EQUAL ZEROS
               MOVE '?'            TO  WRK-B052-CCARTR-IMOV-REG-NULL
           END-IF.

           STRING  RUECB052 (1:140)
                   WRK-B052-CMUN-RURAL-NULL
                   RUECB052 (141:3)
                   WRK-B052-CCEP-NULL
                   RUECB052 (144:2)
                   WRK-B052-CCEP-COMPL-NULL
                   RUECB052 (146:209)
                   WRK-B052-RTPO-IMOV-ARREN-NULL
                   RUECB052 (355:50)
                   WRK-B052-RTPO-DOCTO-NULL
                   RUECB052 (405:33)
                   WRK-B052-CCARTR-IMOV-REG-NULL
                   RUECB052 (438:399)
                   DELIMITED BY SIZE
           INTO    FD-REG-RUEC052S.

           PERFORM 7270-GRAVAR-SAIDA-RUEC052S.

      *---------------------------------------------------------------*
       4510-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       4520-CLOSE-CSR25-RUECB052       SECTION.
      *---------------------------------------------------------------*

           EXEC SQL
             CLOSE CSR25-RUECB052
           END-EXEC.

           IF  ( SQLCODE               NOT EQUAL ZEROS             )
           OR  ( SQLWARN0              EQUAL    'W'                )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'RUECB052'         TO  ERR-DBD-TAB
               MOVE 'CLOSE   '         TO  ERR-FUN-COMANDO
               MOVE SQLCODE            TO  ERR-SQL-CODE
               MOVE '4520'             TO  ERR-LOCAL
               MOVE SPACES             TO  ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       4520-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       4600-OPEN-CSR26-RUECB053        SECTION.
      *---------------------------------------------------------------*

           EXEC SQL
             OPEN CSR26-RUECB053
           END-EXEC.

           IF  ( SQLCODE               NOT EQUAL ZEROS             )
           OR  ( SQLWARN0              EQUAL    'W'                )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'RUECB053'         TO  ERR-DBD-TAB
               MOVE 'OPEN    '         TO  ERR-FUN-COMANDO
               MOVE SQLCODE            TO  ERR-SQL-CODE
               MOVE '4600'             TO  ERR-LOCAL
               MOVE SPACES             TO  ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       4600-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       4610-FETCH-CSR26-RUECB053       SECTION.
      *---------------------------------------------------------------*

           EXEC SQL
             FETCH CSR26-RUECB053
             INTO
               :RUECB053.CCONTR,
               :RUECB053.CPRODT-RURAL,
               :RUECB053.CSEQ-HIST-RBNHO,
               :RUECB053.QRBNHO-IMOV,
               :RUECB053.RESPEC-RBNHO,
               :RUECB053.HULT-ATULZ-EFETU,
               :RUECB053.CFUNC-BDSCO
           END-EXEC.


           IF  ( SQLCODE               NOT EQUAL ZEROS AND +100    )
           OR  ( SQLWARN0              EQUAL    'W'                )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'RUECB053'         TO  ERR-DBD-TAB
               MOVE 'FETCH   '         TO  ERR-FUN-COMANDO
               MOVE SQLCODE            TO  ERR-SQL-CODE
               MOVE '4610'             TO  ERR-LOCAL
               MOVE SPACES             TO  ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO  WRK-FIM-RUEC053S
               GO                      TO  4610-99-FIM
           END-IF.

           ADD 1                       TO ACU-LIDOS-RUEC053S.

           PERFORM 7280-GRAVAR-SAIDA-RUEC053S.

      *---------------------------------------------------------------*
       4610-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       4620-CLOSE-CSR26-RUECB053       SECTION.
      *---------------------------------------------------------------*

           EXEC SQL
             CLOSE CSR26-RUECB053
           END-EXEC.

           IF  ( SQLCODE               NOT EQUAL ZEROS             )
           OR  ( SQLWARN0              EQUAL    'W'                )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'RUECB053'         TO  ERR-DBD-TAB
               MOVE 'CLOSE   '         TO  ERR-FUN-COMANDO
               MOVE SQLCODE            TO  ERR-SQL-CODE
               MOVE '4620'             TO  ERR-LOCAL
               MOVE SPACES             TO  ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       4620-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       4700-OPEN-CSR27-RUECB054        SECTION.
      *---------------------------------------------------------------*

           EXEC SQL
             OPEN CSR27-RUECB054
           END-EXEC.

           IF  ( SQLCODE               NOT EQUAL ZEROS             )
           OR  ( SQLWARN0              EQUAL    'W'                )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'RUECB054'         TO  ERR-DBD-TAB
               MOVE 'OPEN    '         TO  ERR-FUN-COMANDO
               MOVE SQLCODE            TO  ERR-SQL-CODE
               MOVE '4700'             TO  ERR-LOCAL
               MOVE SPACES             TO  ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       4700-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       4710-FETCH-CSR27-RUECB054       SECTION.
      *---------------------------------------------------------------*

           EXEC SQL
             FETCH CSR27-RUECB054
             INTO
               :RUECB054.CCONTR,
               :RUECB054.CPRODT-RURAL,
               :RUECB054.URENDA-AGROP,
               :RUECB054.QRENDA-AGROP,
               :RUECB054.VRENDA-AGROP,
               :RUECB054.PRBATE-RENDA-CLI,
               :RUECB054.HULT-ATULZ-EFETU,
               :RUECB054.CFUNC-BDSCO
           END-EXEC.


           IF  ( SQLCODE               NOT EQUAL ZEROS AND +100    )
           OR  ( SQLWARN0              EQUAL    'W'                )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'RUECB054'         TO  ERR-DBD-TAB
               MOVE 'FETCH   '         TO  ERR-FUN-COMANDO
               MOVE SQLCODE            TO  ERR-SQL-CODE
               MOVE '4710'             TO  ERR-LOCAL
               MOVE SPACES             TO  ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO  WRK-FIM-RUEC054S
               GO                      TO  4710-99-FIM
           END-IF.

           ADD 1                       TO ACU-LIDOS-RUEC054S.

           PERFORM 7290-GRAVAR-SAIDA-RUEC054S.

      *---------------------------------------------------------------*
       4710-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       4720-CLOSE-CSR27-RUECB054       SECTION.
      *---------------------------------------------------------------*

           EXEC SQL
             CLOSE CSR27-RUECB054
           END-EXEC.

           IF  ( SQLCODE               NOT EQUAL ZEROS             )
           OR  ( SQLWARN0              EQUAL    'W'                )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'RUECB054'         TO  ERR-DBD-TAB
               MOVE 'CLOSE   '         TO  ERR-FUN-COMANDO
               MOVE SQLCODE            TO  ERR-SQL-CODE
               MOVE '4720'             TO  ERR-LOCAL
               MOVE SPACES             TO  ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       4720-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       4800-OPEN-CSR28-RUECB064        SECTION.
      *---------------------------------------------------------------*

           EXEC SQL
             OPEN CSR28-RUECB064
           END-EXEC.

           IF  ( SQLCODE               NOT EQUAL ZEROS             )
           OR  ( SQLWARN0              EQUAL    'W'                )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'RUECB064'         TO  ERR-DBD-TAB
               MOVE 'OPEN    '         TO  ERR-FUN-COMANDO
               MOVE SQLCODE            TO  ERR-SQL-CODE
               MOVE '4800'             TO  ERR-LOCAL
               MOVE SPACES             TO  ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       4800-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       4810-FETCH-CSR28-RUECB064       SECTION.
      *---------------------------------------------------------------*

           INITIALIZE WRK-RUEC064S-NULL
                     REPLACING ALPHANUMERIC DATA BY SPACES.

           INITIALIZE RUECB064.


           EXEC SQL
             FETCH CSR28-RUECB064
             INTO
               :RUECB064.CCONTR,
               :RUECB064.CSEQ-HIST-DEVLC,
               :RUECB064.DDEVLC-CONTR,
               :RUECB064.VFINAN
                 :WRK-B064-VFINAN-N,
               :RUECB064.CFUNC-BDSCO,
               :RUECB064.RMOTVO-DEVLC-CONTR
           END-EXEC.


           IF  ( SQLCODE               NOT EQUAL ZEROS AND +100    )
           OR  ( SQLWARN0              EQUAL    'W'                )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'RUECB064'         TO  ERR-DBD-TAB
               MOVE 'FETCH   '         TO  ERR-FUN-COMANDO
               MOVE SQLCODE            TO  ERR-SQL-CODE
               MOVE '4810'             TO  ERR-LOCAL
               MOVE SPACES             TO  ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO  WRK-FIM-RUEC064S
               GO                      TO  4810-99-FIM
           END-IF.

           ADD 1                       TO ACU-LIDOS-RUEC064S.

           IF  WRK-B064-VFINAN-N       NOT EQUAL ZEROS
               MOVE '?'                TO  WRK-B064-VFINAN-NULL
           END-IF.


           STRING  RUECB064 (01:25)
                   WRK-B064-VFINAN-NULL
                   RUECB064 (26:1192)
                   DELIMITED BY SIZE
           INTO    FD-REG-RUEC064S.


           PERFORM 7300-GRAVAR-SAIDA-RUEC064S.

      *---------------------------------------------------------------*
       4810-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       4820-CLOSE-CSR28-RUECB064       SECTION.
      *---------------------------------------------------------------*

           EXEC SQL
             CLOSE CSR28-RUECB064
           END-EXEC.

           IF  ( SQLCODE               NOT EQUAL ZEROS             )
           OR  ( SQLWARN0              EQUAL    'W'                )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'RUECB064'         TO  ERR-DBD-TAB
               MOVE 'CLOSE   '         TO  ERR-FUN-COMANDO
               MOVE SQLCODE            TO  ERR-SQL-CODE
               MOVE '4820'             TO  ERR-LOCAL
               MOVE SPACES             TO  ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       4820-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       4900-OPEN-CSR29-RUECB065        SECTION.
      *---------------------------------------------------------------*

           EXEC SQL
             OPEN CSR29-RUECB065
           END-EXEC.

           IF  ( SQLCODE               NOT EQUAL ZEROS             )
           OR  ( SQLWARN0              EQUAL    'W'                )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'RUECB065'         TO  ERR-DBD-TAB
               MOVE 'OPEN    '         TO  ERR-FUN-COMANDO
               MOVE SQLCODE            TO  ERR-SQL-CODE
               MOVE '4900'             TO  ERR-LOCAL
               MOVE SPACES             TO  ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       4900-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       4910-FETCH-CSR29-RUECB065       SECTION.
      *---------------------------------------------------------------*

           INITIALIZE WRK-RUEC065S-NULL
                     REPLACING ALPHANUMERIC DATA BY SPACES.

           INITIALIZE RUECB065.


           EXEC SQL
             FETCH CSR29-RUECB065
             INTO
               :RUECB065.CCONTR-CREDT-RURAL,
               :RUECB065.NSEQ-REG-CARTR,
               :RUECB065.CCARTR-CONTR-RURAL,
               :RUECB065.ICARTR-CONTR-RURAL,
               :RUECB065.CMATR-CARTR-RURAL,
               :RUECB065.CMUN-CARTR,
               :RUECB065.CREG-PNHOR-RURAL
                 :WRK-B065-CREG-PNHOR-RURAL-N,
               :RUECB065.CLIVRO-PNHOR-RURAL
                 :WRK-B065-CLIVRO-PNHOR-RURAL-N,
               :RUECB065.DREG-PNHOR-RURAL
                 :WRK-B065-DREG-PNHOR-RURAL-N,
               :RUECB065.CREG-HIPOT-RURAL
                 :WRK-B065-CREG-HIPOT-RURAL-N,
               :RUECB065.CLIVRO-HIPOT-RURAL
                 :WRK-B065-CLIVRO-HIPOT-RURAL-N,
               :RUECB065.DREG-HIPOT-RURAL
                 :WRK-B065-DREG-HIPOT-RURAL-N,
               :RUECB065.CSIT-PNHOR-HIPOT,
               :RUECB065.CORIGE-CDULA,
               :RUECB065.DINCL-CARTR-RURAL
                 :WRK-B065-DINCL-CARTR-RURAL-N,
               :RUECB065.CFUNC-BDSCO-INCL,
               :RUECB065.HULT-ATULZ
                 :WRK-B065-HULT-ATULZ-N,
               :RUECB065.CFUNC-ULT-ATULZ
                 :WRK-B065-CFUNC-ULT-ATULZ-N
           END-EXEC.


           IF  ( SQLCODE               NOT EQUAL ZEROS AND +100    )
           OR  ( SQLWARN0              EQUAL    'W'                )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'RUECB065'         TO  ERR-DBD-TAB
               MOVE 'FETCH   '         TO  ERR-FUN-COMANDO
               MOVE SQLCODE            TO  ERR-SQL-CODE
               MOVE '4910'             TO  ERR-LOCAL
               MOVE SPACES             TO  ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO  WRK-FIM-RUEC065S
               GO                      TO  4910-99-FIM
           END-IF.

           ADD 1                       TO ACU-LIDOS-RUEC065S.

           IF  WRK-B065-CREG-PNHOR-RURAL-N    NOT EQUAL ZEROS
               MOVE '?'             TO  WRK-B065-CREG-PNHOR-RURAL-NULL
           END-IF.

           IF  WRK-B065-CLIVRO-PNHOR-RURAL-N    NOT EQUAL ZEROS
               MOVE '?'             TO  WRK-B065-CLIVRO-PNHOR-RURAL-NU
           END-IF.

           IF  WRK-B065-DREG-PNHOR-RURAL-N    NOT EQUAL ZEROS
               MOVE '?'             TO  WRK-B065-DREG-PNHOR-RURAL-NULL
           END-IF.

           IF  WRK-B065-CREG-HIPOT-RURAL-N    NOT EQUAL ZEROS
               MOVE '?'             TO  WRK-B065-CREG-HIPOT-RURAL-NULL
           END-IF.

           IF  WRK-B065-CLIVRO-HIPOT-RURAL-N    NOT EQUAL ZEROS
               MOVE '?'             TO  WRK-B065-CLIVRO-HIPOT-RURAL-NU
           END-IF.

           IF  WRK-B065-DREG-HIPOT-RURAL-N    NOT EQUAL ZEROS
               MOVE '?'             TO  WRK-B065-DREG-HIPOT-RURAL-NULL
           END-IF.

           IF  WRK-B065-DINCL-CARTR-RURAL-N    NOT EQUAL ZEROS
               MOVE '?'             TO  WRK-B065-DINCL-CARTR-RURAL-NUL
           END-IF.

           IF  WRK-B065-HULT-ATULZ-N    NOT EQUAL ZEROS
               MOVE '?'             TO  WRK-B065-HULT-ATULZ-NULL
           END-IF.

           IF  WRK-B065-CFUNC-ULT-ATULZ-N    NOT EQUAL ZEROS
               MOVE '?'             TO  WRK-B065-CFUNC-ULT-ATULZ-NULL
           END-IF.



           STRING  RUECB065 (01:364)
                   WRK-B065-CREG-PNHOR-RURAL-NULL
                   RUECB065 (365:10)
                   WRK-B065-CLIVRO-PNHOR-RURAL-NU
                   RUECB065 (375:10)
                   WRK-B065-DREG-PNHOR-RURAL-NULL
                   RUECB065 (385:10)
                   WRK-B065-CREG-HIPOT-RURAL-NULL
                   RUECB065 (395:10)
                   WRK-B065-CLIVRO-HIPOT-RURAL-NU
                   RUECB065 (405:10)
                   WRK-B065-DREG-HIPOT-RURAL-NULL
                   RUECB065 (415:12)
                   WRK-B065-DINCL-CARTR-RURAL-NUL
                   RUECB065 (427:31)
                   WRK-B065-HULT-ATULZ-NULL
                   RUECB065 (458:05)
                   WRK-B065-CFUNC-ULT-ATULZ-NULL
                   DELIMITED BY SIZE
           INTO    FD-REG-RUEC065S.


           PERFORM 7310-GRAVAR-SAIDA-RUEC065S.

      *---------------------------------------------------------------*
       4910-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       4920-CLOSE-CSR29-RUECB065       SECTION.
      *---------------------------------------------------------------*

           EXEC SQL
             CLOSE CSR29-RUECB065
           END-EXEC.

           IF  ( SQLCODE               NOT EQUAL ZEROS             )
           OR  ( SQLWARN0              EQUAL    'W'                )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'RUECB065'         TO  ERR-DBD-TAB
               MOVE 'CLOSE   '         TO  ERR-FUN-COMANDO
               MOVE SQLCODE            TO  ERR-SQL-CODE
               MOVE '4920'             TO  ERR-LOCAL
               MOVE SPACES             TO  ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       4920-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       5000-OPEN-CSR30-RUECB066        SECTION.
      *---------------------------------------------------------------*

           EXEC SQL
             OPEN CSR30-RUECB066
           END-EXEC.

           IF  ( SQLCODE               NOT EQUAL ZEROS             )
           OR  ( SQLWARN0              EQUAL    'W'                )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'RUECB066'         TO  ERR-DBD-TAB
               MOVE 'OPEN    '         TO  ERR-FUN-COMANDO
               MOVE SQLCODE            TO  ERR-SQL-CODE
               MOVE '5000'             TO  ERR-LOCAL
               MOVE SPACES             TO  ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       5000-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       5010-FETCH-CSR30-RUECB066       SECTION.
      *---------------------------------------------------------------*

           INITIALIZE WRK-RUEC066S-NULL
                     REPLACING ALPHANUMERIC DATA BY SPACES.

           INITIALIZE RUECB066.


           EXEC SQL
             FETCH CSR30-RUECB066
             INTO
               :RUECB066.CAG-BCRIA,
               :RUECB066.CCDULA-CREDT-RURAL,
               :RUECB066.NSEQ-OBS-LICEN,
               :RUECB066.ROBS-LICEN-AMBTL,
               :RUECB066.DINCL-OBS-LICEN
                 :WRK-B066-DINCL-OBS-LICEN-N,
               :RUECB066.CFUNC-BDSCO-INCL,
               :RUECB066.HULT-ATULZ
                 :WRK-B066-HULT-ATULZ-N,
               :RUECB066.CFUNC-ULT-ATULZ
                 :WRK-B066-CFUNC-ULT-ATULZ-N
           END-EXEC.


           IF  ( SQLCODE               NOT EQUAL ZEROS AND +100    )
           OR  ( SQLWARN0              EQUAL    'W'                )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'RUECB066'         TO  ERR-DBD-TAB
               MOVE 'FETCH   '         TO  ERR-FUN-COMANDO
               MOVE SQLCODE            TO  ERR-SQL-CODE
               MOVE '5010'             TO  ERR-LOCAL
               MOVE SPACES             TO  ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO  WRK-FIM-RUEC066S
               GO                      TO  5010-99-FIM
           END-IF.

           ADD 1                       TO ACU-LIDOS-RUEC066S.

           IF  WRK-B066-DINCL-OBS-LICEN-N   NOT EQUAL ZEROS
               MOVE '?'             TO  WRK-B066-DINCL-OBS-LICEN-NULL
           END-IF.

           IF  WRK-B066-HULT-ATULZ-N   NOT EQUAL ZEROS
               MOVE '?'             TO  WRK-B066-HULT-ATULZ-NULL
           END-IF.

           IF  WRK-B066-CFUNC-ULT-ATULZ-N   NOT EQUAL ZEROS
               MOVE '?'             TO  WRK-B066-CFUNC-ULT-ATULZ-NULL
           END-IF.



           STRING  RUECB066 (01:95)
                   WRK-B066-DINCL-OBS-LICEN-NULL
                   RUECB066 (96:31)
                   WRK-B066-HULT-ATULZ-NULL
                   RUECB066 (127:05)
                   WRK-B066-CFUNC-ULT-ATULZ-NULL
                   DELIMITED BY SIZE
           INTO    FD-REG-RUEC066S.


           PERFORM 7320-GRAVAR-SAIDA-RUEC066S.

      *---------------------------------------------------------------*
       5010-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       5020-CLOSE-CSR30-RUECB066       SECTION.
      *---------------------------------------------------------------*

           EXEC SQL
             CLOSE CSR30-RUECB066
           END-EXEC.

           IF  ( SQLCODE               NOT EQUAL ZEROS             )
           OR  ( SQLWARN0              EQUAL    'W'                )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'RUECB066'         TO  ERR-DBD-TAB
               MOVE 'CLOSE   '         TO  ERR-FUN-COMANDO
               MOVE SQLCODE            TO  ERR-SQL-CODE
               MOVE '5020'             TO  ERR-LOCAL
               MOVE SPACES             TO  ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       5020-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       5100-OPEN-CSR31-RUECB073        SECTION.
      *---------------------------------------------------------------*

           EXEC SQL
             OPEN CSR31-RUECB073
           END-EXEC.

           IF  ( SQLCODE               NOT EQUAL ZEROS             )
           OR  ( SQLWARN0              EQUAL    'W'                )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'RUECB073'         TO  ERR-DBD-TAB
               MOVE 'OPEN    '         TO  ERR-FUN-COMANDO
               MOVE SQLCODE            TO  ERR-SQL-CODE
               MOVE '5100'             TO  ERR-LOCAL
               MOVE SPACES             TO  ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       5100-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       5110-FETCH-CSR31-RUECB073       SECTION.
      *---------------------------------------------------------------*


           EXEC SQL
             FETCH CSR31-RUECB073
             INTO
               :RUECB073.CCONTR-CREDT-RURAL,
               :RUECB073.DVCTO-PCELA-CONTR,
               :RUECB073.NLIM-OPER,
               :RUECB073.DEVNTO-LIM-OPER,
               :RUECB073.RMANUT-CONTR,
               :RUECB073.VUTLZD-LIM-OPER,
               :RUECB073.DULT-ATULZ,
               :RUECB073.CUSUAR-SENHA
           END-EXEC.


           IF  ( SQLCODE               NOT EQUAL ZEROS AND +100    )
           OR  ( SQLWARN0              EQUAL    'W'                )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'RUECB073'         TO  ERR-DBD-TAB
               MOVE 'FETCH   '         TO  ERR-FUN-COMANDO
               MOVE SQLCODE            TO  ERR-SQL-CODE
               MOVE '5110'             TO  ERR-LOCAL
               MOVE SPACES             TO  ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO  WRK-FIM-RUEC073S
               GO                      TO  5110-99-FIM
           END-IF.

           ADD 1                       TO ACU-LIDOS-RUEC073S.

           PERFORM 7330-GRAVAR-SAIDA-RUEC073S.

      *---------------------------------------------------------------*
       5110-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       5120-CLOSE-CSR31-RUECB073       SECTION.
      *---------------------------------------------------------------*

           EXEC SQL
             CLOSE CSR31-RUECB073
           END-EXEC.

           IF  ( SQLCODE               NOT EQUAL ZEROS             )
           OR  ( SQLWARN0              EQUAL    'W'                )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'RUECB073'         TO  ERR-DBD-TAB
               MOVE 'CLOSE   '         TO  ERR-FUN-COMANDO
               MOVE SQLCODE            TO  ERR-SQL-CODE
               MOVE '5120'             TO  ERR-LOCAL
               MOVE SPACES             TO  ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       5120-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       5200-OPEN-CSR32-RUECB074        SECTION.
      *---------------------------------------------------------------*

           EXEC SQL
             OPEN CSR32-RUECB074
           END-EXEC.

           IF  ( SQLCODE               NOT EQUAL ZEROS             )
           OR  ( SQLWARN0              EQUAL    'W'                )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'RUECB074'         TO  ERR-DBD-TAB
               MOVE 'OPEN    '         TO  ERR-FUN-COMANDO
               MOVE SQLCODE            TO  ERR-SQL-CODE
               MOVE '5200'             TO  ERR-LOCAL
               MOVE SPACES             TO  ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       5200-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       5210-FETCH-CSR32-RUECB074       SECTION.
      *---------------------------------------------------------------*

           INITIALIZE WRK-RUEC074S-NULL
                     REPLACING ALPHANUMERIC DATA BY SPACES.

           INITIALIZE RUECB074.


           EXEC SQL
             FETCH CSR32-RUECB074
             INTO
               :RUECB074.CJUNC-DEPDC,
               :RUECB074.CCDULA-CREDT-RURAL,
               :RUECB074.CSEQ-PSSOA,
               :RUECB074.ICONJG-PSSOA,
               :RUECB074.CCPF-CONJG-PSSOA,
               :RUECB074.CCTRL-CONJG-PSSOA,
               :RUECB074.CTPO-DOCTO-PSSOA,
               :RUECB074.CDOCTO-PSSOA,
               :RUECB074.CORG-EMISR-DOCTO
                 :WRK-B074-CORG-EMISR-DOCTO-N,
               :RUECB074.CUND-ORG-EMISR
                 :WRK-B074-CUND-ORG-EMISR-N,
               :RUECB074.CNAC,
               :RUECB074.CSGL-UF
                 :WRK-B074-CSGL-UF-N,
               :RUECB074.INAT-ESTRG,
               :RUECB074.CCATEG-PROFS-CREDT,
               :RUECB074.CPROFS-PSSOA
           END-EXEC.


           IF  ( SQLCODE               NOT EQUAL ZEROS AND +100    )
           OR  ( SQLWARN0              EQUAL    'W'                )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'RUECB074'         TO  ERR-DBD-TAB
               MOVE 'FETCH   '         TO  ERR-FUN-COMANDO
               MOVE SQLCODE            TO  ERR-SQL-CODE
               MOVE '5210'             TO  ERR-LOCAL
               MOVE SPACES             TO  ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO  WRK-FIM-RUEC074S
               GO                      TO  5210-99-FIM
           END-IF.

           ADD 1                       TO ACU-LIDOS-RUEC074S.

           IF  WRK-B074-CORG-EMISR-DOCTO-N   NOT EQUAL ZEROS
               MOVE '?'             TO  WRK-B074-CORG-EMISR-DOCTO-NULL
           END-IF.

           IF  WRK-B074-CUND-ORG-EMISR-N   NOT EQUAL ZEROS
               MOVE '?'             TO  WRK-B074-CUND-ORG-EMISR-NULL
           END-IF.

           IF  WRK-B074-CSGL-UF-N   NOT EQUAL ZEROS
               MOVE '?'             TO  WRK-B074-CSGL-UF-NULL
           END-IF.



           STRING  RUECB074 (01:98)
                   WRK-B074-CORG-EMISR-DOCTO-NULL
                   RUECB074 (99:02)
                   WRK-B074-CUND-ORG-EMISR-NULL
                   RUECB074 (101:04)
                   WRK-B074-CSGL-UF-NULL
                   RUECB074 (105:46)
                   DELIMITED BY SIZE
           INTO    FD-REG-RUEC074S.


           PERFORM 7340-GRAVAR-SAIDA-RUEC074S.

      *---------------------------------------------------------------*
       5210-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       5220-CLOSE-CSR32-RUECB074       SECTION.
      *---------------------------------------------------------------*

           EXEC SQL
             CLOSE CSR32-RUECB074
           END-EXEC.

           IF  ( SQLCODE               NOT EQUAL ZEROS             )
           OR  ( SQLWARN0              EQUAL    'W'                )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'RUECB074'         TO  ERR-DBD-TAB
               MOVE 'CLOSE   '         TO  ERR-FUN-COMANDO
               MOVE SQLCODE            TO  ERR-SQL-CODE
               MOVE '5220'             TO  ERR-LOCAL
               MOVE SPACES             TO  ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       5220-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       5300-OPEN-CSR33-RUECB075        SECTION.
      *---------------------------------------------------------------*

           EXEC SQL
             OPEN CSR33-RUECB075
           END-EXEC.

           IF  ( SQLCODE               NOT EQUAL ZEROS             )
           OR  ( SQLWARN0              EQUAL    'W'                )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'RUECB075'         TO  ERR-DBD-TAB
               MOVE 'OPEN    '         TO  ERR-FUN-COMANDO
               MOVE SQLCODE            TO  ERR-SQL-CODE
               MOVE '5300'             TO  ERR-LOCAL
               MOVE SPACES             TO  ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       5300-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       5310-FETCH-CSR33-RUECB075       SECTION.
      *---------------------------------------------------------------*

           INITIALIZE WRK-RUEC075S-NULL
                     REPLACING ALPHANUMERIC DATA BY SPACES.

           INITIALIZE RUECB075.


           EXEC SQL
             FETCH CSR33-RUECB075
             INTO
               :RUECB075.CCONTR-CREDT-RURAL,
               :RUECB075.CSEQ-PSSOA,
               :RUECB075.ICONJG-PSSOA,
               :RUECB075.CCPF-CONJG-PSSOA,
               :RUECB075.CCTRL-CONJG-PSSOA,
               :RUECB075.CTPO-DOCTO-PSSOA,
               :RUECB075.CDOCTO-PSSOA,
               :RUECB075.CORG-EMISR-DOCTO
                 :WRK-B075-CORG-EMISR-DOCTO-N,
               :RUECB075.CUND-ORG-EMISR
                 :WRK-B075-CUND-ORG-EMISR-N,
               :RUECB075.CNAC,
               :RUECB075.CSGL-UF
                 :WRK-B075-CSGL-UF-N,
               :RUECB075.INAT-ESTRG,
               :RUECB075.CCATEG-PROFS-CREDT,
               :RUECB075.CPROFS-PSSOA
           END-EXEC.


           IF  ( SQLCODE               NOT EQUAL ZEROS AND +100    )
           OR  ( SQLWARN0              EQUAL    'W'                )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'RUECB075'         TO  ERR-DBD-TAB
               MOVE 'FETCH   '         TO  ERR-FUN-COMANDO
               MOVE SQLCODE            TO  ERR-SQL-CODE
               MOVE '5310'             TO  ERR-LOCAL
               MOVE SPACES             TO  ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO  WRK-FIM-RUEC075S
               GO                      TO  5310-99-FIM
           END-IF.

           ADD 1                       TO ACU-LIDOS-RUEC075S.

           IF  WRK-B075-CORG-EMISR-DOCTO-N   NOT EQUAL ZEROS
               MOVE '?'             TO  WRK-B075-CORG-EMISR-DOCTO-NULL
           END-IF.

           IF  WRK-B075-CUND-ORG-EMISR-N   NOT EQUAL ZEROS
               MOVE '?'             TO  WRK-B075-CUND-ORG-EMISR-NULL
           END-IF.

           IF  WRK-B075-CSGL-UF-N   NOT EQUAL ZEROS
               MOVE '?'             TO  WRK-B075-CSGL-UF-NULL
           END-IF.



           STRING  RUECB075 (01:95)
                   WRK-B075-CORG-EMISR-DOCTO-NULL
                   RUECB075 (96:02)
                   WRK-B075-CUND-ORG-EMISR-NULL
                   RUECB075 (98:04)
                   WRK-B075-CSGL-UF-NULL
                   RUECB075 (102:46)
                   DELIMITED BY SIZE
           INTO    FD-REG-RUEC075S.


           PERFORM 7350-GRAVAR-SAIDA-RUEC075S.

      *---------------------------------------------------------------*
       5310-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       5320-CLOSE-CSR33-RUECB075       SECTION.
      *---------------------------------------------------------------*

           EXEC SQL
             CLOSE CSR33-RUECB075
           END-EXEC.

           IF  ( SQLCODE               NOT EQUAL ZEROS             )
           OR  ( SQLWARN0              EQUAL    'W'                )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'RUECB075'         TO  ERR-DBD-TAB
               MOVE 'CLOSE   '         TO  ERR-FUN-COMANDO
               MOVE SQLCODE            TO  ERR-SQL-CODE
               MOVE '5320'             TO  ERR-LOCAL
               MOVE SPACES             TO  ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       5320-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       5400-OPEN-CSR34-RUECB096        SECTION.
      *---------------------------------------------------------------*

           EXEC SQL
             OPEN CSR34-RUECB096
           END-EXEC.

           IF  ( SQLCODE               NOT EQUAL ZEROS             )
           OR  ( SQLWARN0              EQUAL    'W'                )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'RUECB096'         TO  ERR-DBD-TAB
               MOVE 'OPEN    '         TO  ERR-FUN-COMANDO
               MOVE SQLCODE            TO  ERR-SQL-CODE
               MOVE '5400'             TO  ERR-LOCAL
               MOVE SPACES             TO  ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       5400-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       5410-FETCH-CSR34-RUECB096       SECTION.
      *---------------------------------------------------------------*

           INITIALIZE WRK-RUEC096S-NULL
                     REPLACING ALPHANUMERIC DATA BY SPACES.

           INITIALIZE RUECB096.


           EXEC SQL
             FETCH CSR34-RUECB096
             INTO
               :RUECB096.CCONTR-CREDT-RURAL,
               :RUECB096.NSEQ-HIST-SIT,
               :RUECB096.CINDCD-SIT-CDULA
                 :WRK-B096-CINDCD-SIT-CDULA-N,
               :RUECB096.CREFT-BACEN
                 :WRK-B096-CREFT-BACEN-N,
               :RUECB096.DULT-ATULZ,
               :RUECB096.CFUNC-ULT-ATULZ,
               :RUECB096.CREFT-RECOR,
               :RUECB096.CMOTVO-EXCL-RECOR
           END-EXEC.


           IF  ( SQLCODE               NOT EQUAL ZEROS AND +100    )
           OR  ( SQLWARN0              EQUAL    'W'                )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'RUECB096'         TO  ERR-DBD-TAB
               MOVE 'FETCH   '         TO  ERR-FUN-COMANDO
               MOVE SQLCODE            TO  ERR-SQL-CODE
               MOVE '5410'             TO  ERR-LOCAL
               MOVE SPACES             TO  ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO  WRK-FIM-RUEC096S
               GO                      TO  5410-99-FIM
           END-IF.

           ADD 1                       TO ACU-LIDOS-RUEC096S.

           IF  WRK-B096-CINDCD-SIT-CDULA-N   NOT EQUAL ZEROS
               MOVE '?'             TO  WRK-B096-CINDCD-SIT-CDULA-NULL
           END-IF.

           IF  WRK-B096-CREFT-BACEN-N   NOT EQUAL ZEROS
               MOVE '?'             TO  WRK-B096-CREFT-BACEN-NULL
           END-IF.


           STRING  RUECB096 (01:08)
                   WRK-B096-CINDCD-SIT-CDULA-NULL
                   RUECB096 (09:05)
                   WRK-B096-CREFT-BACEN-NULL
                   RUECB096 (14:29)
                   DELIMITED BY SIZE
           INTO    FD-REG-RUEC096S.


           PERFORM 7360-GRAVAR-SAIDA-RUEC096S.

      *---------------------------------------------------------------*
       5410-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       5420-CLOSE-CSR34-RUECB096       SECTION.
      *---------------------------------------------------------------*

           EXEC SQL
             CLOSE CSR34-RUECB096
           END-EXEC.

           IF  ( SQLCODE               NOT EQUAL ZEROS             )
           OR  ( SQLWARN0              EQUAL    'W'                )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'RUECB096'         TO  ERR-DBD-TAB
               MOVE 'CLOSE   '         TO  ERR-FUN-COMANDO
               MOVE SQLCODE            TO  ERR-SQL-CODE
               MOVE '5420'             TO  ERR-LOCAL
               MOVE SPACES             TO  ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       5420-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

AC0613*---------------------------------------------------------------*
AC0613 5500-OPEN-CSR35-RUECB097        SECTION.
AC0613*---------------------------------------------------------------*
AC0613
AC0613     EXEC SQL
AC0613       OPEN CSR35-RUECB097
AC0613     END-EXEC.
AC0613
AC0613     IF  ( SQLCODE               NOT EQUAL ZEROS             )
AC0613     OR  ( SQLWARN0              EQUAL    'W'                )
AC0613         MOVE 'DB2'              TO  ERR-TIPO-ACESSO
AC0613         MOVE 'RUECB097'         TO  ERR-DBD-TAB
AC0613         MOVE 'OPEN    '         TO  ERR-FUN-COMANDO
AC0613         MOVE SQLCODE            TO  ERR-SQL-CODE
AC0613         MOVE '5500'             TO  ERR-LOCAL
AC0613         MOVE SPACES             TO  ERR-SEGM
AC0613         PERFORM 9999-ROTINA-ERRO
AC0613     END-IF.
AC0613
AC0613*---------------------------------------------------------------*
AC0613 5500-99-FIM.                    EXIT.
AC0613*---------------------------------------------------------------*
AC0613
AC0613*---------------------------------------------------------------*
AC0613 5510-FETCH-CSR35-RUECB097       SECTION.
AC0613*---------------------------------------------------------------*
AC0613
AC0613     INITIALIZE WRK-RUEC097S-NULL
AC0613               REPLACING ALPHANUMERIC DATA BY SPACES.
AC0613
AC0613     INITIALIZE RUECB097.
AC0613
AC0613
AC0613     EXEC SQL
AC0613       FETCH CSR35-RUECB097
AC0613       INTO
AC0613         :RUECB097.CJUNC-DEPDC,
AC0613         :RUECB097.CCDULA-CREDT-RURAL,
AC0613         :RUECB097.NITEM-COMCZ,
AC0613         :RUECB097.CPROTR-RURAL,
AC0613         :RUECB097.CNOTA-FSCAL-VDA,
AC0613         :RUECB097.DNOTA-FSCAL-VDA,
AC0613         :RUECB097.QUND-NOTA-FSCAL,
AC0613         :RUECB097.VUNTAR-NOTA-FSCAL,
AC0613         :RUECB097.CUND-MEDD,
AC0613         :RUECB097.CMUN-RURAL,
AC0613         :RUECB097.DINIC-SAFRA,
AC0613         :RUECB097.DFIM-SAFRA,
AC0613         :RUECB097.QUND-FINCD-RURAL,
AC0613         :RUECB097.VPRECO-MIN-RURAL,
AC0613         :RUECB097.VUTLZD-NOTA-FSCAL,
AC0613         :RUECB097.CSIT-VNCLO,
AC0613         :RUECB097.HINCL,
AC0613         :RUECB097.HULT-ATULZ,
AC0613         :RUECB097.CUSUAR-RESP
AC0613              :WRK-B097-CUSUAR-N,
AC0613         :RUECB097.CMODLD-RURAL
AC0613              :WRK-B097-CMODLD-N,
AC0613         :RUECB097.CPRODT-RURAL
AC0613              :WRK-B097-CPRODT-N,
AC0613         :RUECB097.CESTCA-SAFRA
AC0613              :WRK-B097-CESTCA-N
AC0613     END-EXEC.
AC0613
AC0613
AC0613     IF  ( SQLCODE               NOT EQUAL ZEROS AND +100    )
AC0613     OR  ( SQLWARN0              EQUAL    'W'                )
AC0613         MOVE 'DB2'              TO  ERR-TIPO-ACESSO
AC0613         MOVE 'RUECB097'         TO  ERR-DBD-TAB
AC0613         MOVE 'FETCH   '         TO  ERR-FUN-COMANDO
AC0613         MOVE SQLCODE            TO  ERR-SQL-CODE
AC0613         MOVE '5510'             TO  ERR-LOCAL
AC0613         MOVE SPACES             TO  ERR-SEGM
AC0613         PERFORM 9999-ROTINA-ERRO
AC0613     END-IF.
AC0613
AC0613     IF  SQLCODE                 EQUAL +100
AC0613         MOVE 'S'                TO  WRK-FIM-RUEC097S
AC0613         GO                      TO  5510-99-FIM
AC0613     END-IF.
AC0613
AC0613     ADD 1                       TO ACU-LIDOS-RUEC097S.
AC0613
AC0613     IF  WRK-B097-CUSUAR-N             NOT EQUAL ZEROS
AC0613         MOVE '?'             TO  WRK-B097-CUSUAR-NULL
AC0613     END-IF.
AC0613
AC0613     IF  WRK-B097-CMODLD-N             NOT EQUAL ZEROS
AC0613         MOVE '?'             TO  WRK-B097-CMODLD-NULL
AC0613     END-IF.
AC0613
AC0613     IF  WRK-B097-CPRODT-N             NOT EQUAL ZEROS
AC0613         MOVE '?'             TO  WRK-B097-CPRODT-NULL
AC0613     END-IF.
AC0613
AC0613     IF  WRK-B097-CESTCA-N             NOT EQUAL ZEROS
AC0613         MOVE '?'             TO  WRK-B097-CESTCA-NULL
AC0613     END-IF.
AC0613
AC0613     STRING  RUECB097 (01:151)
AC0613             WRK-B097-CUSUAR-NULL
AC0613             RUECB097 (152:03)
AC0613             WRK-B097-CMODLD-NULL
AC0613             RUECB097 (155:02)
AC0613             WRK-B097-CPRODT-NULL
AC0613             RUECB097 (157:01)
AC0613             WRK-B097-CESTCA-NULL
AC0613             DELIMITED BY SIZE
AC0613     INTO    FD-REG-RUEC097S.
AC0613
AC0613
AC0613     PERFORM 7370-GRAVAR-SAIDA-RUEC097S.
AC0613
AC0613*---------------------------------------------------------------*
AC0613 5510-99-FIM.                    EXIT.
AC0613*---------------------------------------------------------------*
AC0613
AC0613*---------------------------------------------------------------*
AC0613 5520-CLOSE-CSR35-RUECB097       SECTION.
AC0613*---------------------------------------------------------------*
AC0613
AC0613     EXEC SQL
AC0613       CLOSE CSR35-RUECB097
AC0613     END-EXEC.
AC0613
AC0613     IF  ( SQLCODE               NOT EQUAL ZEROS             )
AC0613     OR  ( SQLWARN0              EQUAL    'W'                )
AC0613         MOVE 'DB2'              TO  ERR-TIPO-ACESSO
AC0613         MOVE 'RUECB097'         TO  ERR-DBD-TAB
AC0613         MOVE 'CLOSE   '         TO  ERR-FUN-COMANDO
AC0613         MOVE SQLCODE            TO  ERR-SQL-CODE
AC0613         MOVE '5520'             TO  ERR-LOCAL
AC0613         MOVE SPACES             TO  ERR-SEGM
AC0613         PERFORM 9999-ROTINA-ERRO
AC0613     END-IF.
AC0613
AC0613*---------------------------------------------------------------*
AC0613 5520-99-FIM.                    EXIT.
AC0613*---------------------------------------------------------------*

AC0613*---------------------------------------------------------------*
AC0613 5600-OPEN-CSR36-RUECB099        SECTION.
AC0613*---------------------------------------------------------------*
AC0613
AC0613     EXEC SQL
AC0613       OPEN CSR36-RUECB099
AC0613     END-EXEC.
AC0613
AC0613     IF  ( SQLCODE               NOT EQUAL ZEROS             )
AC0613     OR  ( SQLWARN0              EQUAL    'W'                )
AC0613         MOVE 'DB2'              TO  ERR-TIPO-ACESSO
AC0613         MOVE 'RUECB099'         TO  ERR-DBD-TAB
AC0613         MOVE 'OPEN    '         TO  ERR-FUN-COMANDO
AC0613         MOVE SQLCODE            TO  ERR-SQL-CODE
AC0613         MOVE '5600'             TO  ERR-LOCAL
AC0613         MOVE SPACES             TO  ERR-SEGM
AC0613         PERFORM 9999-ROTINA-ERRO
AC0613     END-IF.
AC0613
AC0613*---------------------------------------------------------------*
AC0613 5600-99-FIM.                    EXIT.
AC0613*---------------------------------------------------------------*
AC0613
AC0613*---------------------------------------------------------------*
AC0613 5610-FETCH-CSR36-RUECB099       SECTION.
AC0613*---------------------------------------------------------------*
AC0613
AC0613     INITIALIZE WRK-RUEC099S-NULL
AC0613               REPLACING ALPHANUMERIC DATA BY SPACES.
AC0613
AC0613     INITIALIZE RUECB099.
AC0613
AC0613
AC0613     EXEC SQL
AC0613       FETCH CSR36-RUECB099
AC0613       INTO
AC0613         :RUECB099.CCONTR-CREDT-RURAL,
AC0613         :RUECB099.NENVIO-RECOR,
AC0613         :RUECB099.CCNPJ-ENTID-RESP,
AC0613         :RUECB099.CFLIAL-CNPJ-ENTID,
AC0613         :RUECB099.CCTRL-CNPJ-ENTID,
AC0613         :RUECB099.CCNPJ-PARTC,
AC0613         :RUECB099.CFLIAL-CNPJ-PARTC,
AC0613         :RUECB099.CCTRL-CNPJ-PARTC ,
AC0613         :RUECB099.CSIT-OPER-RURAL,
AC0613         :RUECB099.DCDULA
AC0613              :WRK-B099-DCDULA-N,
AC0613         :RUECB099.DVCTO-CONTR
AC0613              :WRK-B099-DVCTO-N,
AC0613         :RUECB099.CCDULA-RECOR,
AC0613         :RUECB099.CTPO-INSTT-CREDT,
AC0613         :RUECB099.VFINAN
AC0613              :WRK-B099-VFINAN-N,
AC0613         :RUECB099.CCATEG-PROTR-RECOR
AC0613              :WRK-B099-CCATEG-N,
AC0613         :RUECB099.CPROG-RURAL
AC0613              :WRK-B099-CPROG-N,
AC0613         :RUECB099.CTPO-FONTE-RECOR,
AC0613         :RUECB099.CMUN-REFT-BACEN
AC0613              :WRK-B099-CMUN-N,
AC0613         :RUECB099.CEMPTO-BACEN,
AC0613         :RUECB099.CSIST-PROD,
AC0613         :RUECB099.VPCELA-LIBRC
AC0613              :WRK-B099-LIBRC-N,
AC0613         :RUECB099.VPCELA-REC-PPRIO
AC0613              :WRK-B099-REC-N,
AC0613         :RUECB099.PALIQT-PROAGRO,
AC0613         :RUECB099.PJURO-ENCAR-FINCR,
AC0613         :RUECB099.CTPO-ENCAR-COMPL,
AC0613         :RUECB099.CTSORO-NACIO,
AC0613         :RUECB099.PRISCO-TSORO-NACIO,
AC0613         :RUECB099.PRISCO-FUNDO-CNSTT,
AC0613         :RUECB099.QAREA-RECOR,
AC0613         :RUECB099.QUND-FINCD-RURAL
AC0613              :WRK-B099-FINCD-N,
AC0613         :RUECB099.CUND-MEDD-FINCD,
AC0613         :RUECB099.QUND-PROD-PROVV
AC0613              :WRK-B099-PROD-N,
AC0613         :RUECB099.CUND-MEDD-PROVV,
AC0613         :RUECB099.DINIC-SAFRA
AC0613              :WRK-B099-DINIC-N,
AC0613         :RUECB099.DFIM-SAFRA
AC0613              :WRK-B099-DFIM-N,
AC0613         :RUECB099.CTPO-GARNT-RECOR
AC0613              :WRK-B099-GARNT-N,
AC0613         :RUECB099.VRECTA-BRUTA-RECOR,
AC0613         :RUECB099.DMOVTO-RECOR
AC0613     END-EXEC.
AC0613
AC0613
AC0613     IF  ( SQLCODE               NOT EQUAL ZEROS AND +100    )
AC0613     OR  ( SQLWARN0              EQUAL    'W'                )
AC0613         MOVE 'DB2'              TO  ERR-TIPO-ACESSO
AC0613         MOVE 'RUECB099'         TO  ERR-DBD-TAB
AC0613         MOVE 'FETCH   '         TO  ERR-FUN-COMANDO
AC0613         MOVE SQLCODE            TO  ERR-SQL-CODE
AC0613         MOVE '5610'             TO  ERR-LOCAL
AC0613         MOVE SPACES             TO  ERR-SEGM
AC0613         PERFORM 9999-ROTINA-ERRO
AC0613     END-IF.
AC0613
AC0613     IF  SQLCODE                 EQUAL +100
AC0613         MOVE 'S'                TO  WRK-FIM-RUEC099S
AC0613         GO                      TO  5610-99-FIM
AC0613     END-IF.
AC0613
AC0613     ADD 1                       TO ACU-LIDOS-RUEC099S.
AC0613
AC0613     IF  WRK-B099-DCDULA-N              NOT EQUAL ZEROS
AC0613         MOVE '?'             TO  WRK-B099-DCDULA-NULL
AC0613     END-IF.
AC0613
AC0613     IF  WRK-B099-DVCTO-N               NOT EQUAL ZEROS
AC0613         MOVE '?'             TO  WRK-B099-DVCTO-NULL
AC0613     END-IF.
AC0613
AC0613     IF  WRK-B099-VFINAN-N      NOT EQUAL ZEROS
AC0613         MOVE '?'             TO  WRK-B099-VFINAN-NULL
AC0613     END-IF.
AC0613
AC0613     IF  WRK-B099-CCATEG-N              NOT EQUAL ZEROS
AC0613         MOVE '?'             TO  WRK-B099-CCATEG-NULL
AC0613     END-IF.
AC0613
AC0613     IF  WRK-B099-CPROG-N               NOT EQUAL ZEROS
AC0613         MOVE '?'             TO  WRK-B099-CPROG-NULL
AC0613     END-IF.
AC0613
AC0613     IF  WRK-B099-CMUN-N                NOT EQUAL ZEROS
AC0613         MOVE '?'             TO  WRK-B099-CMUN-NULL
AC0613     END-IF.
AC0613
AC0613     IF  WRK-B099-LIBRC-N               NOT EQUAL ZEROS
AC0613         MOVE '?'             TO  WRK-B099-LIBRC-NULL
AC0613     END-IF.
AC0613
AC0613     IF  WRK-B099-REC-N                 NOT EQUAL ZEROS
AC0613         MOVE '?'             TO  WRK-B099-REC-NULL
AC0613     END-IF.
AC0613
AC0613     IF  WRK-B099-FINCD-N               NOT EQUAL ZEROS
AC0613         MOVE '?'             TO  WRK-B099-FINCD-NULL
AC0613     END-IF.
AC0613
AC0613     IF  WRK-B099-PROD-N                NOT EQUAL ZEROS
AC0613         MOVE '?'             TO  WRK-B099-PROD-NULL
AC0613     END-IF.
AC0613
AC0613     IF  WRK-B099-DINIC-N               NOT EQUAL ZEROS
AC0613         MOVE '?'             TO  WRK-B099-DINIC-NULL
AC0613     END-IF.
AC0613
AC0613     IF  WRK-B099-DFIM-N                NOT EQUAL ZEROS
AC0613         MOVE '?'             TO  WRK-B099-DFIM-NULL
AC0613     END-IF.
AC0613
AC0613     IF  WRK-B099-GARNT-N               NOT EQUAL ZEROS
AC0613         MOVE '?'             TO  WRK-B099-GARNT-NULL
AC0613     END-IF.
AC0613
AC0613     STRING  RUECB099 (01:40)
AC0613             WRK-B099-DCDULA-NULL
AC0613             RUECB099 (41:10)
AC0613             WRK-B099-DVCTO-NULL
AC0613             RUECB099 (51:27)
AC0613             WRK-B099-VFINAN-NULL
AC0613             RUECB099 (78:04)
AC0613             WRK-B099-CCATEG-NULL
AC0613             RUECB099 (82:04)
AC0613             WRK-B099-CPROG-NULL
AC0613             RUECB099 (86:08)
AC0613             WRK-B099-CMUN-NULL
AC0613             RUECB099 (94:31)
AC0613             WRK-B099-LIBRC-NULL
AC0613             RUECB099 (125:08)
AC0613             WRK-B099-REC-NULL
AC0613             RUECB099 (133:41)
AC0613             WRK-B099-FINCD-NULL
AC0613             RUECB099 (174:08)
AC0613             WRK-B099-PROD-NULL
AC0613             RUECB099 (182:06)
AC0613             WRK-B099-DINIC-NULL
AC0613             RUECB099 (188:04)
AC0613             WRK-B099-DFIM-NULL
AC0613             RUECB099 (192:02)
AC0613             WRK-B099-GARNT-NULL
AC0613             RUECB099 (194:19)
AC0613             DELIMITED BY SIZE
AC0613     INTO    FD-REG-RUEC099S.
AC0613
AC0613     PERFORM 7380-GRAVAR-SAIDA-RUEC099S.
AC0613
AC0613*---------------------------------------------------------------*
AC0613 5610-99-FIM.                    EXIT.
AC0613*---------------------------------------------------------------*
AC0613
AC0613*---------------------------------------------------------------*
AC0613 5620-CLOSE-CSR36-RUECB099       SECTION.
AC0613*---------------------------------------------------------------*
AC0613
AC0613     EXEC SQL
AC0613       CLOSE CSR36-RUECB099
AC0613     END-EXEC.
AC0613
AC0613     IF  ( SQLCODE               NOT EQUAL ZEROS             )
AC0613     OR  ( SQLWARN0              EQUAL    'W'                )
AC0613         MOVE 'DB2'              TO  ERR-TIPO-ACESSO
AC0613         MOVE 'RUECB099'         TO  ERR-DBD-TAB
AC0613         MOVE 'CLOSE   '         TO  ERR-FUN-COMANDO
AC0613         MOVE SQLCODE            TO  ERR-SQL-CODE
AC0613         MOVE '5620'             TO  ERR-LOCAL
AC0613         MOVE SPACES             TO  ERR-SEGM
AC0613         PERFORM 9999-ROTINA-ERRO
AC0613     END-IF.
AC0613
AC0613*---------------------------------------------------------------*
AC0613 5620-99-FIM.                    EXIT.
AC0613*---------------------------------------------------------------*

AC0613*---------------------------------------------------------------*
AC0613 5700-OPEN-CSR37-RUECB09A        SECTION.
AC0613*---------------------------------------------------------------*
AC0613
AC0613     EXEC SQL
AC0613       OPEN CSR37-RUECB09A
AC0613     END-EXEC.
AC0613
AC0613     IF  ( SQLCODE               NOT EQUAL ZEROS             )
AC0613     OR  ( SQLWARN0              EQUAL    'W'                )
AC0613         MOVE 'DB2'              TO  ERR-TIPO-ACESSO
AC0613         MOVE 'RUECB09A'         TO  ERR-DBD-TAB
AC0613         MOVE 'OPEN    '         TO  ERR-FUN-COMANDO
AC0613         MOVE SQLCODE            TO  ERR-SQL-CODE
AC0613         MOVE '5700'             TO  ERR-LOCAL
AC0613         MOVE SPACES             TO  ERR-SEGM
AC0613         PERFORM 9999-ROTINA-ERRO
AC0613     END-IF.
AC0613
AC0613*---------------------------------------------------------------*
AC0613 5700-99-FIM.                    EXIT.
AC0613*---------------------------------------------------------------*
AC0613
AC0613*---------------------------------------------------------------*
AC0613 5710-FETCH-CSR37-RUECB09A       SECTION.
AC0613*---------------------------------------------------------------*
AC0613
AC0613     INITIALIZE WRK-RUEC09AS-NULL
AC0613               REPLACING ALPHANUMERIC DATA BY SPACES.
AC0613
AC0613     INITIALIZE RUECB09A.
AC0613
AC0613
AC0613     EXEC SQL
AC0613       FETCH CSR37-RUECB09A
AC0613       INTO
AC0613         :RUECB09A.CCONTR-CREDT-RURAL,
AC0613         :RUECB09A.NENVIO-RECOR,
AC0613         :RUECB09A.NREG-MSGEM-RECOR,
AC0613         :RUECB09A.CSIT-MSGEM
AC0613              :WRK-B09A-CSIT-N,
AC0613         :RUECB09A.HINCL
AC0613              :WRK-B09A-HINCL-N,
AC0613         :RUECB09A.WCONTD-MSGEM-RURAL
AC0613     END-EXEC.
AC0613
AC0613
AC0613     IF  ( SQLCODE               NOT EQUAL ZEROS AND +100    )
AC0613     OR  ( SQLWARN0              EQUAL    'W'                )
AC0613         MOVE 'DB2'              TO  ERR-TIPO-ACESSO
AC0613         MOVE 'RUECB09A'         TO  ERR-DBD-TAB
AC0613         MOVE 'FETCH   '         TO  ERR-FUN-COMANDO
AC0613         MOVE SQLCODE            TO  ERR-SQL-CODE
AC0613         MOVE '5710'             TO  ERR-LOCAL
AC0613         MOVE SPACES             TO  ERR-SEGM
AC0613         PERFORM 9999-ROTINA-ERRO
AC0613     END-IF.
AC0613
AC0613     IF  SQLCODE                 EQUAL +100
AC0613         MOVE 'S'                TO  WRK-FIM-RUEC09AS
AC0613         GO                      TO  5710-99-FIM
AC0613     END-IF.
AC0613
AC0613     ADD 1                       TO ACU-LIDOS-RUEC09AS.
AC0613
AC0613     IF  WRK-B09A-CSIT-N               NOT EQUAL ZEROS
AC0613         MOVE '?'             TO  WRK-B09A-CSIT-NULL
AC0613     END-IF.
AC0613
AC0613     IF  WRK-B09A-HINCL-N              NOT EQUAL ZEROS
AC0613         MOVE '?'             TO  WRK-B09A-HINCL-NULL
AC0613     END-IF.
AC0613
AC0613     STRING  RUECB09A (01:12)
AC0613             WRK-B09A-CSIT-NULL
AC0613             RUECB09A (13:26)
AC0613             WRK-B09A-HINCL-NULL
AC0613             RUECB09A (39:32502)
AC0613             DELIMITED BY SIZE
AC0613     INTO    FD-REG-RUEC09AS.
AC0613
AC0613     PERFORM 7390-GRAVAR-SAIDA-RUEC09AS.
AC0613
AC0613*---------------------------------------------------------------*
AC0613 5710-99-FIM.                    EXIT.
AC0613*---------------------------------------------------------------*
AC0613
AC0613*---------------------------------------------------------------*
AC0613 5720-CLOSE-CSR37-RUECB09A       SECTION.
AC0613*---------------------------------------------------------------*
AC0613
AC0613     EXEC SQL
AC0613       CLOSE CSR37-RUECB09A
AC0613     END-EXEC.
AC0613
AC0613     IF  ( SQLCODE               NOT EQUAL ZEROS             )
AC0613     OR  ( SQLWARN0              EQUAL    'W'                )
AC0613         MOVE 'DB2'              TO  ERR-TIPO-ACESSO
AC0613         MOVE 'RUECB09A'         TO  ERR-DBD-TAB
AC0613         MOVE 'CLOSE   '         TO  ERR-FUN-COMANDO
AC0613         MOVE SQLCODE            TO  ERR-SQL-CODE
AC0613         MOVE '5720'             TO  ERR-LOCAL
AC0613         MOVE SPACES             TO  ERR-SEGM
AC0613         PERFORM 9999-ROTINA-ERRO
AC0613     END-IF.
AC0613
AC0613*---------------------------------------------------------------*
AC0613 5720-99-FIM.                    EXIT.
AC0613*---------------------------------------------------------------*

AC0613*---------------------------------------------------------------*
AC0613 5800-OPEN-CSR38-RUECB09B        SECTION.
AC0613*---------------------------------------------------------------*
AC0613
AC0613     EXEC SQL
AC0613       OPEN CSR38-RUECB09B
AC0613     END-EXEC.
AC0613
AC0613     IF  ( SQLCODE               NOT EQUAL ZEROS             )
AC0613     OR  ( SQLWARN0              EQUAL    'W'                )
AC0613         MOVE 'DB2'              TO  ERR-TIPO-ACESSO
AC0613         MOVE 'RUECB09B'         TO  ERR-DBD-TAB
AC0613         MOVE 'OPEN    '         TO  ERR-FUN-COMANDO
AC0613         MOVE SQLCODE            TO  ERR-SQL-CODE
AC0613         MOVE '5800'             TO  ERR-LOCAL
AC0613         MOVE SPACES             TO  ERR-SEGM
AC0613         PERFORM 9999-ROTINA-ERRO
AC0613     END-IF.
AC0613
AC0613*---------------------------------------------------------------*
AC0613 5800-99-FIM.                    EXIT.
AC0613*---------------------------------------------------------------*
AC0613
AC0613*---------------------------------------------------------------*
AC0613 5810-FETCH-CSR38-RUECB09B       SECTION.
AC0613*---------------------------------------------------------------*
AC0613
AC0613     INITIALIZE WRK-RUEC09BS-NULL
AC0613               REPLACING ALPHANUMERIC DATA BY SPACES.
AC0613
AC0613     INITIALIZE RUECB09B.
AC0613
AC0613
AC0613     EXEC SQL
AC0613       FETCH CSR38-RUECB09B
AC0613       INTO
AC0613         :RUECB09B.CCONTR-CREDT-RURAL,
AC0613         :RUECB09B.NENVIO-RECOR,
AC0613         :RUECB09B.HULT-ATULZ,
AC0613         :RUECB09B.CMSGEM-RECOR
AC0613              :WRK-B09B-CMSGEM-N,
AC0613         :RUECB09B.HINCL
AC0613              :WRK-B09B-HINCL-N,
AC0613         :RUECB09B.CCTRL-RQUIS-FINCR
AC0613              :WRK-B09B-CCTRL-N,
AC0613         :RUECB09B.CUNIC-MSGEM
AC0613              :WRK-B09B-CUNIC-N,
AC0613         :RUECB09B.CREFT-RECOR
AC0613              :WRK-B09B-CREFT-N,
AC0613         :RUECB09B.CSIT-MSGEM
AC0613              :WRK-B09B-CSIT-N,
AC0613         :RUECB09B.CUSUAR-RESP
AC0613              :WRK-B09B-CUSUAR-N
AC0613     END-EXEC.
AC0613
AC0613     IF  ( SQLCODE               NOT EQUAL ZEROS AND +100    )
AC0613     OR  ( SQLWARN0              EQUAL    'W'                )
AC0613         MOVE 'DB2'              TO  ERR-TIPO-ACESSO
AC0613         MOVE 'RUECB09B'         TO  ERR-DBD-TAB
AC0613         MOVE 'FETCH   '         TO  ERR-FUN-COMANDO
AC0613         MOVE SQLCODE            TO  ERR-SQL-CODE
AC0613         MOVE '5810'             TO  ERR-LOCAL
AC0613         MOVE SPACES             TO  ERR-SEGM
AC0613         PERFORM 9999-ROTINA-ERRO
AC0613     END-IF.
AC0613
AC0613     IF  SQLCODE                 EQUAL +100
AC0613         MOVE 'S'                TO  WRK-FIM-RUEC09BS
AC0613         GO                      TO  5810-99-FIM
AC0613     END-IF.
AC0613
AC0613     ADD 1                       TO ACU-LIDOS-RUEC09BS.
AC0613
AC0613     IF  WRK-B09B-CMSGEM-N             NOT EQUAL ZEROS
AC0613         MOVE '?'             TO  WRK-B09B-CMSGEM-NULL
AC0613     END-IF.
AC0613
AC0613     IF  WRK-B09B-HINCL-N              NOT EQUAL ZEROS
AC0613         MOVE '?'             TO  WRK-B09B-HINCL-NULL
AC0613     END-IF.
AC0613
AC0613     IF  WRK-B09B-CCTRL-N              NOT EQUAL ZEROS
AC0613         MOVE '?'             TO  WRK-B09B-CCTRL-NULL
AC0613     END-IF.
AC0613
AC0613     IF  WRK-B09B-CUNIC-N              NOT EQUAL ZEROS
AC0613         MOVE '?'             TO  WRK-B09B-CUNIC-NULL
AC0613     END-IF.
AC0613
AC0613     IF  WRK-B09B-CREFT-N              NOT EQUAL ZEROS
AC0613         MOVE '?'             TO  WRK-B09B-CREFT-NULL
AC0613     END-IF.
AC0613
AC0613     IF  WRK-B09B-CSIT-N               NOT EQUAL ZEROS
AC0613         MOVE '?'             TO  WRK-B09B-CSIT-NULL
AC0613     END-IF.
AC0613
AC0613     IF  WRK-B09B-CUSUAR-N             NOT EQUAL ZEROS
AC0613         MOVE '?'             TO  WRK-B09B-CUSUAR-NULL
AC0613     END-IF.
AC0613
AC0613     STRING  RUECB09B (01:43)
AC0613             WRK-B09B-CMSGEM-NULL
AC0613             RUECB09B (44:26)
AC0613             WRK-B09B-HINCL-NULL
AC0613             RUECB09B (70:20)
AC0613             WRK-B09B-CCTRL-NULL
AC0613             RUECB09B (90:23)
AC0613             WRK-B09B-CUNIC-NULL
AC0613             RUECB09B (113:11)
AC0613             WRK-B09B-CREFT-NULL
AC0613             RUECB09B (124:01)
AC0613             WRK-B09B-CSIT-NULL
AC0613             RUECB09B (125:09)
AC0613             WRK-B09B-CUSUAR-NULL
AC0613             DELIMITED BY SIZE
AC0613     INTO    FD-REG-RUEC09BS.
AC0613
AC0613     PERFORM 7400-GRAVAR-SAIDA-RUEC09BS.
AC0613
AC0613*---------------------------------------------------------------*
AC0613 5810-99-FIM.                    EXIT.
AC0613*---------------------------------------------------------------*
AC0613
AC0613*---------------------------------------------------------------*
AC0613 5820-CLOSE-CSR38-RUECB09B       SECTION.
AC0613*---------------------------------------------------------------*
AC0613
AC0613     EXEC SQL
AC0613       CLOSE CSR38-RUECB09B
AC0613     END-EXEC.
AC0613
AC0613     IF  ( SQLCODE               NOT EQUAL ZEROS             )
AC0613     OR  ( SQLWARN0              EQUAL    'W'                )
AC0613         MOVE 'DB2'              TO  ERR-TIPO-ACESSO
AC0613         MOVE 'RUECB09B'         TO  ERR-DBD-TAB
AC0613         MOVE 'CLOSE   '         TO  ERR-FUN-COMANDO
AC0613         MOVE SQLCODE            TO  ERR-SQL-CODE
AC0613         MOVE '5820'             TO  ERR-LOCAL
AC0613         MOVE SPACES             TO  ERR-SEGM
AC0613         PERFORM 9999-ROTINA-ERRO
AC0613     END-IF.
AC0613
AC0613*---------------------------------------------------------------*
AC0613 5820-99-FIM.                    EXIT.
AC0613*---------------------------------------------------------------*

AC0613*---------------------------------------------------------------*
AC0613 5900-OPEN-CSR39-RUECB09D        SECTION.
AC0613*---------------------------------------------------------------*
AC0613
AC0613     EXEC SQL
AC0613       OPEN CSR39-RUECB09D
AC0613     END-EXEC.
AC0613
AC0613     IF  ( SQLCODE               NOT EQUAL ZEROS             )
AC0613     OR  ( SQLWARN0              EQUAL    'W'                )
AC0613         MOVE 'DB2'              TO  ERR-TIPO-ACESSO
AC0613         MOVE 'RUECB09D'         TO  ERR-DBD-TAB
AC0613         MOVE 'OPEN    '         TO  ERR-FUN-COMANDO
AC0613         MOVE SQLCODE            TO  ERR-SQL-CODE
AC0613         MOVE '5900'             TO  ERR-LOCAL
AC0613         MOVE SPACES             TO  ERR-SEGM
AC0613         PERFORM 9999-ROTINA-ERRO
AC0613     END-IF.
AC0613
AC0613*---------------------------------------------------------------*
AC0613 5900-99-FIM.                    EXIT.
AC0613*---------------------------------------------------------------*
AC0613
AC0613*---------------------------------------------------------------*
AC0613 5910-FETCH-CSR39-RUECB09D       SECTION.
AC0613*---------------------------------------------------------------*
AC0613
AC0613     INITIALIZE WRK-RUEC09DS-NULL
AC0613               REPLACING ALPHANUMERIC DATA BY SPACES.
AC0613
AC0613     INITIALIZE RUECB09D.
AC0613
AC0613
AC0613     EXEC SQL
AC0613       FETCH CSR39-RUECB09D
AC0613       INTO
AC0613         :RUECB09D.CCONTR-CREDT-RURAL,
AC0613         :RUECB09D.NENVIO-RECOR,
AC0613         :RUECB09D.NPCELA-CONTR,
AC0613         :RUECB09D.DVCTO-PCELA-CONTR
AC0613              :WRK-B09D-DVCTO-N,
AC0613         :RUECB09D.VPCELA-RURAL
AC0613     END-EXEC.
AC0613
AC0613
AC0613     IF  ( SQLCODE               NOT EQUAL ZEROS AND +100    )
AC0613     OR  ( SQLWARN0              EQUAL    'W'                )
AC0613         MOVE 'DB2'              TO  ERR-TIPO-ACESSO
AC0613         MOVE 'RUECB09D'         TO  ERR-DBD-TAB
AC0613         MOVE 'FETCH   '         TO  ERR-FUN-COMANDO
AC0613         MOVE SQLCODE            TO  ERR-SQL-CODE
AC0613         MOVE '5910'             TO  ERR-LOCAL
AC0613         MOVE SPACES             TO  ERR-SEGM
AC0613         PERFORM 9999-ROTINA-ERRO
AC0613     END-IF.
AC0613
AC0613     IF  SQLCODE                 EQUAL +100
AC0613         MOVE 'S'                TO  WRK-FIM-RUEC09DS
AC0613         GO                      TO  5910-99-FIM
AC0613     END-IF.
AC0613
AC0613     ADD 1                       TO ACU-LIDOS-RUEC09DS.
AC0613
AC0613     IF  WRK-B09D-DVCTO-N              NOT EQUAL ZEROS
AC0613         MOVE '?'             TO  WRK-B09D-DVCTO-NULL
AC0613     END-IF.
AC0613
AC0613     STRING  RUECB09D (01:20)
AC0613             WRK-B09D-DVCTO-NULL
AC0613             RUECB09D (21:08)
AC0613             DELIMITED BY SIZE
AC0613     INTO    FD-REG-RUEC09DS.
AC0613
AC0613     PERFORM 7410-GRAVAR-SAIDA-RUEC09DS.
AC0613
AC0613*---------------------------------------------------------------*
AC0613 5910-99-FIM.                    EXIT.
AC0613*---------------------------------------------------------------*
AC0613
AC0613*---------------------------------------------------------------*
AC0613 5920-CLOSE-CSR39-RUECB09D       SECTION.
AC0613*---------------------------------------------------------------*
AC0613
AC0613     EXEC SQL
AC0613       CLOSE CSR39-RUECB09D
AC0613     END-EXEC.
AC0613
AC0613     IF  ( SQLCODE               NOT EQUAL ZEROS             )
AC0613     OR  ( SQLWARN0              EQUAL    'W'                )
AC0613         MOVE 'DB2'              TO  ERR-TIPO-ACESSO
AC0613         MOVE 'RUECB09D'         TO  ERR-DBD-TAB
AC0613         MOVE 'CLOSE   '         TO  ERR-FUN-COMANDO
AC0613         MOVE SQLCODE            TO  ERR-SQL-CODE
AC0613         MOVE '5920'             TO  ERR-LOCAL
AC0613         MOVE SPACES             TO  ERR-SEGM
AC0613         PERFORM 9999-ROTINA-ERRO
AC0613     END-IF.
AC0613
AC0613*---------------------------------------------------------------*
AC0613 5920-99-FIM.                    EXIT.
AC0613*---------------------------------------------------------------*

AC0613*---------------------------------------------------------------*
AC0613 6000-OPEN-CSR40-RUECB09E        SECTION.
AC0613*---------------------------------------------------------------*
AC0613
AC0613     EXEC SQL
AC0613       OPEN CSR40-RUECB09E
AC0613     END-EXEC.
AC0613
AC0613     IF  ( SQLCODE               NOT EQUAL ZEROS             )
AC0613     OR  ( SQLWARN0              EQUAL    'W'                )
AC0613         MOVE 'DB2'              TO  ERR-TIPO-ACESSO
AC0613         MOVE 'RUECB09E'         TO  ERR-DBD-TAB
AC0613         MOVE 'OPEN    '         TO  ERR-FUN-COMANDO
AC0613         MOVE SQLCODE            TO  ERR-SQL-CODE
AC0613         MOVE '6000'             TO  ERR-LOCAL
AC0613         MOVE SPACES             TO  ERR-SEGM
AC0613         PERFORM 9999-ROTINA-ERRO
AC0613     END-IF.
AC0613
AC0613*---------------------------------------------------------------*
AC0613 6000-99-FIM.                    EXIT.
AC0613*---------------------------------------------------------------*
AC0613
AC0613*---------------------------------------------------------------*
AC0613 6010-FETCH-CSR40-RUECB09E       SECTION.
AC0613*---------------------------------------------------------------*
AC0613
AC0613     INITIALIZE WRK-RUEC09ES-NULL
AC0613               REPLACING ALPHANUMERIC DATA BY SPACES.
AC0613
AC0613     INITIALIZE RUECB09E.
AC0613
AC0613
AC0613     EXEC SQL
AC0613       FETCH CSR40-RUECB09E
AC0613       INTO
AC0613         :RUECB09E.CCONTR-CREDT-RURAL,
AC0613         :RUECB09E.NENVIO-RECOR,
AC0613         :RUECB09E.NPSSOA-CONTR,
AC0613         :RUECB09E.CTPO-RESP-PSSOA
AC0613              :WRK-B09E-RESP-N,
AC0613         :RUECB09E.CTPO-PSSOA
AC0613              :WRK-B09E-PSSOA-N,
AC0613         :RUECB09E.CCNPJ-CPF
AC0613              :WRK-B09E-CCNPJ-N,
AC0613         :RUECB09E.CFLIAL-CNPJ
AC0613              :WRK-B09E-CFLIAL-N,
AC0613         :RUECB09E.CCTRL-CNPJ-CPF
AC0613              :WRK-B09E-CCTRL-N
AC0613     END-EXEC.
AC0613
AC0613
AC0613     IF  ( SQLCODE               NOT EQUAL ZEROS AND +100    )
AC0613     OR  ( SQLWARN0              EQUAL    'W'                )
AC0613         MOVE 'DB2'              TO  ERR-TIPO-ACESSO
AC0613         MOVE 'RUECB09E'         TO  ERR-DBD-TAB
AC0613         MOVE 'FETCH   '         TO  ERR-FUN-COMANDO
AC0613         MOVE SQLCODE            TO  ERR-SQL-CODE
AC0613         MOVE '6010'             TO  ERR-LOCAL
AC0613         MOVE SPACES             TO  ERR-SEGM
AC0613         PERFORM 9999-ROTINA-ERRO
AC0613     END-IF.
AC0613
AC0613     IF  SQLCODE                 EQUAL +100
AC0613         MOVE 'S'                TO  WRK-FIM-RUEC09ES
AC0613         GO                      TO  6010-99-FIM
AC0613     END-IF.
AC0613
AC0613     ADD 1                       TO ACU-LIDOS-RUEC09ES.
AC0613
AC0613     IF  WRK-B09E-RESP-N               NOT EQUAL ZEROS
AC0613         MOVE '?'             TO  WRK-B09E-RESP-NULL
AC0613     END-IF.
AC0613
AC0613     IF  WRK-B09E-PSSOA-N              NOT EQUAL ZEROS
AC0613         MOVE '?'             TO  WRK-B09E-PSSOA-NULL
AC0613     END-IF.
AC0613
AC0613     IF  WRK-B09E-CCNPJ-N              NOT EQUAL ZEROS
AC0613         MOVE '?'             TO  WRK-B09E-CCNPJ-NULL
AC0613     END-IF.
AC0613
AC0613     IF  WRK-B09E-CFLIAL-N             NOT EQUAL ZEROS
AC0613         MOVE '?'             TO  WRK-B09E-CFLIAL-NULL
AC0613     END-IF.
AC0613
AC0613     IF WRK-B09E-CCTRL-N               NOT EQUAL ZEROS
AC0613         MOVE '?'             TO  WRK-B09E-CCTRL-NULL
AC0613     END-IF.
AC0613
AC0613     STRING  RUECB09E (01:11)
AC0613             WRK-B09E-RESP-NULL
AC0613             RUECB09E (12:01)
AC0613             WRK-B09E-PSSOA-NULL
AC0613             RUECB09E (13:05)
AC0613             WRK-B09E-CCNPJ-NULL
AC0613             RUECB09E (18:03)
AC0613             WRK-B09E-CFLIAL-NULL
AC0613             RUECB09E (21:02)
AC0613             WRK-B09E-CCTRL-NULL
AC0613             DELIMITED BY SIZE
AC0613     INTO    FD-REG-RUEC09ES.
AC0613
AC0613     PERFORM 7420-GRAVAR-SAIDA-RUEC09ES.
AC0613
AC0613*---------------------------------------------------------------*
AC0613 6010-99-FIM.                    EXIT.
AC0613*---------------------------------------------------------------*
AC0613
AC0613*---------------------------------------------------------------*
AC0613 6020-CLOSE-CSR40-RUECB09E       SECTION.
AC0613*---------------------------------------------------------------*
AC0613
AC0613     EXEC SQL
AC0613       CLOSE CSR40-RUECB09E
AC0613     END-EXEC.
AC0613
AC0613     IF  ( SQLCODE               NOT EQUAL ZEROS             )
AC0613     OR  ( SQLWARN0              EQUAL    'W'                )
AC0613         MOVE 'DB2'              TO  ERR-TIPO-ACESSO
AC0613         MOVE 'RUECB09E'         TO  ERR-DBD-TAB
AC0613         MOVE 'CLOSE   '         TO  ERR-FUN-COMANDO
AC0613         MOVE SQLCODE            TO  ERR-SQL-CODE
AC0613         MOVE '6020'             TO  ERR-LOCAL
AC0613         MOVE SPACES             TO  ERR-SEGM
AC0613         PERFORM 9999-ROTINA-ERRO
AC0613     END-IF.
AC0613
AC0613*---------------------------------------------------------------*
AC0613 6020-99-FIM.                    EXIT.
AC0613*---------------------------------------------------------------*

AC0613*---------------------------------------------------------------*
AC0613 6100-OPEN-CSR41-RUECB09F        SECTION.
AC0613*---------------------------------------------------------------*
AC0613
AC0613     EXEC SQL
AC0613       OPEN CSR41-RUECB09F
AC0613     END-EXEC.
AC0613
AC0613     IF  ( SQLCODE               NOT EQUAL ZEROS             )
AC0613     OR  ( SQLWARN0              EQUAL    'W'                )
AC0613         MOVE 'DB2'              TO  ERR-TIPO-ACESSO
AC0613         MOVE 'RUECB09F'         TO  ERR-DBD-TAB
AC0613         MOVE 'OPEN    '         TO  ERR-FUN-COMANDO
AC0613         MOVE SQLCODE            TO  ERR-SQL-CODE
AC0613         MOVE '6100'             TO  ERR-LOCAL
AC0613         MOVE SPACES             TO  ERR-SEGM
AC0613         PERFORM 9999-ROTINA-ERRO
AC0613     END-IF.
AC0613
AC0613*---------------------------------------------------------------*
AC0613 6100-99-FIM.                    EXIT.
AC0613*---------------------------------------------------------------*
AC0613
AC0613*---------------------------------------------------------------*
AC0613 6110-FETCH-CSR41-RUECB09F       SECTION.
AC0613*---------------------------------------------------------------*
AC0613
AC0613     INITIALIZE WRK-RUEC09FS-NULL
AC0613               REPLACING ALPHANUMERIC DATA BY SPACES.
AC0613
AC0613     INITIALIZE RUECB09F.
AC0613
AC0613     EXEC SQL
AC0613       FETCH CSR41-RUECB09F
AC0613       INTO
AC0613         :RUECB09F.CCONTR-CREDT-RURAL,
AC0613         :RUECB09F.NENVIO-RECOR,
AC0613         :RUECB09F.NPRODT-RURAL-RECOR,
AC0613         :RUECB09F.CPRODT-RURAL
AC0613              :WRK-B09F-CPRODT-N
AC0613     END-EXEC.
AC0613
AC0613     IF  ( SQLCODE               NOT EQUAL ZEROS AND +100    )
AC0613     OR  ( SQLWARN0              EQUAL    'W'                )
AC0613         MOVE 'DB2'              TO  ERR-TIPO-ACESSO
AC0613         MOVE 'RUECB09F'         TO  ERR-DBD-TAB
AC0613         MOVE 'FETCH   '         TO  ERR-FUN-COMANDO
AC0613         MOVE SQLCODE            TO  ERR-SQL-CODE
AC0613         MOVE '6110'             TO  ERR-LOCAL
AC0613         MOVE SPACES             TO  ERR-SEGM
AC0613         PERFORM 9999-ROTINA-ERRO
AC0613     END-IF.
AC0613
AC0613     IF  SQLCODE                 EQUAL +100
AC0613         MOVE 'S'                TO  WRK-FIM-RUEC09FS
AC0613         GO                      TO  6110-99-FIM
AC0613     END-IF.
AC0613
AC0613     ADD 1                       TO ACU-LIDOS-RUEC09FS.
AC0613
AC0613     IF  WRK-B09F-CPRODT-N             NOT EQUAL ZEROS
AC0613         MOVE '?'             TO  WRK-B09F-CPRODT-NULL
AC0613     END-IF.
AC0613
AC0613     STRING  RUECB09F (01:13)
AC0613             WRK-B09F-CPRODT-NULL
AC0613             DELIMITED BY SIZE
AC0613     INTO    FD-REG-RUEC09FS.
AC0613
AC0613     PERFORM 7430-GRAVAR-SAIDA-RUEC09FS.
AC0613
AC0613*---------------------------------------------------------------*
AC0613 6110-99-FIM.                    EXIT.
AC0613*---------------------------------------------------------------*
AC0613
AC0613*---------------------------------------------------------------*
AC0613 6120-CLOSE-CSR41-RUECB09F       SECTION.
AC0613*---------------------------------------------------------------*
AC0613
AC0613     EXEC SQL
AC0613       CLOSE CSR41-RUECB09F
AC0613     END-EXEC.
AC0613
AC0613     IF  ( SQLCODE               NOT EQUAL ZEROS             )
AC0613     OR  ( SQLWARN0              EQUAL    'W'                )
AC0613         MOVE 'DB2'              TO  ERR-TIPO-ACESSO
AC0613         MOVE 'RUECB09F'         TO  ERR-DBD-TAB
AC0613         MOVE 'CLOSE   '         TO  ERR-FUN-COMANDO
AC0613         MOVE SQLCODE            TO  ERR-SQL-CODE
AC0613         MOVE '6120'             TO  ERR-LOCAL
AC0613         MOVE SPACES             TO  ERR-SEGM
AC0613         PERFORM 9999-ROTINA-ERRO
AC0613     END-IF.
AC0613
AC0613*---------------------------------------------------------------*
AC0613 6120-99-FIM.                    EXIT.
AC0613*---------------------------------------------------------------*

AC0613*---------------------------------------------------------------*
AC0613 6200-OPEN-CSR42-RUECB09G        SECTION.
AC0613*---------------------------------------------------------------*
AC0613
AC0613     EXEC SQL
AC0613       OPEN CSR42-RUECB09G
AC0613     END-EXEC.
AC0613
AC0613     IF  ( SQLCODE               NOT EQUAL ZEROS             )
AC0613     OR  ( SQLWARN0              EQUAL    'W'                )
AC0613         MOVE 'DB2'              TO  ERR-TIPO-ACESSO
AC0613         MOVE 'RUECB09G'         TO  ERR-DBD-TAB
AC0613         MOVE 'OPEN    '         TO  ERR-FUN-COMANDO
AC0613         MOVE SQLCODE            TO  ERR-SQL-CODE
AC0613         MOVE '6200'             TO  ERR-LOCAL
AC0613         MOVE SPACES             TO  ERR-SEGM
AC0613         PERFORM 9999-ROTINA-ERRO
AC0613     END-IF.
AC0613
AC0613*---------------------------------------------------------------*
AC0613 6200-99-FIM.                    EXIT.
AC0613*---------------------------------------------------------------*
AC0613
AC0613*---------------------------------------------------------------*
AC0613 6210-FETCH-CSR42-RUECB09G       SECTION.
AC0613*---------------------------------------------------------------*
AC0613
AC0613     INITIALIZE WRK-RUEC09GS-NULL
AC0613               REPLACING ALPHANUMERIC DATA BY SPACES.
AC0613
AC0613     INITIALIZE RUECB09G.
AC0613
AC0613     EXEC SQL
AC0613       FETCH CSR42-RUECB09G
AC0613       INTO
AC0613         :RUECB09G.CCONTR-CREDT-RURAL,
AC0613         :RUECB09G.NPRODT-RURAL-CSCDO,
AC0613         :RUECB09G.CPRODT-RURAL
AC0613              :WRK-B09G-CPRODT-N,
AC0613         :RUECB09G.DULT-ATULZ
AC0613              :WRK-B09G-DULT-N,
AC0613         :RUECB09G.CUSUAR-RESP
AC0613              :WRK-B09G-CUSUAR-N
AC0613     END-EXEC.
AC0613
AC0613     IF  ( SQLCODE               NOT EQUAL ZEROS AND +100    )
AC0613     OR  ( SQLWARN0              EQUAL    'W'                )
AC0613         MOVE 'DB2'              TO  ERR-TIPO-ACESSO
AC0613         MOVE 'RUECB09G'         TO  ERR-DBD-TAB
AC0613         MOVE 'FETCH   '         TO  ERR-FUN-COMANDO
AC0613         MOVE SQLCODE            TO  ERR-SQL-CODE
AC0613         MOVE '6210'             TO  ERR-LOCAL
AC0613         MOVE SPACES             TO  ERR-SEGM
AC0613         PERFORM 9999-ROTINA-ERRO
AC0613     END-IF.
AC0613
AC0613     IF  SQLCODE                 EQUAL +100
AC0613         MOVE 'S'                TO  WRK-FIM-RUEC09GS
AC0613         GO                      TO  6210-99-FIM
AC0613     END-IF.
AC0613
AC0613     ADD 1                       TO ACU-LIDOS-RUEC09GS.
AC0613
AC0613     IF  WRK-B09G-CPRODT-N             NOT EQUAL ZEROS
AC0613         MOVE '?'             TO  WRK-B09G-CPRODT-NULL
AC0613     END-IF.
AC0613
AC0613     IF  WRK-B09G-DULT-N               NOT EQUAL ZEROS
AC0613         MOVE '?'             TO  WRK-B09G-DULT-NULL
AC0613     END-IF.
AC0613
AC0613     IF  WRK-B09G-CUSUAR-N             NOT EQUAL ZEROS
AC0613         MOVE '?'             TO  WRK-B09G-CUSUAR-NULL
AC0613     END-IF.
AC0613
AC0613     STRING  RUECB09G (01:10)
AC0613             WRK-B09G-CPRODT-NULL
AC0613             RUECB09G (11:10)
AC0613             WRK-B09G-DULT-NULL
AC0613             RUECB09G (21:09)
AC0613             WRK-B09G-CUSUAR-NULL
AC0613             DELIMITED BY SIZE
AC0613     INTO    FD-REG-RUEC09GS.
AC0613
AC0613     PERFORM 7440-GRAVAR-SAIDA-RUEC09GS.
AC0613
AC0613*---------------------------------------------------------------*
AC0613 6210-99-FIM.                    EXIT.
AC0613*---------------------------------------------------------------*
AC0613
AC0613*---------------------------------------------------------------*
AC0613 6220-CLOSE-CSR42-RUECB09G       SECTION.
AC0613*---------------------------------------------------------------*
AC0613
AC0613     EXEC SQL
AC0613       CLOSE CSR42-RUECB09G
AC0613     END-EXEC.
AC0613
AC0613     IF  ( SQLCODE               NOT EQUAL ZEROS             )
AC0613     OR  ( SQLWARN0              EQUAL    'W'                )
AC0613         MOVE 'DB2'              TO  ERR-TIPO-ACESSO
AC0613         MOVE 'RUECB09G'         TO  ERR-DBD-TAB
AC0613         MOVE 'CLOSE   '         TO  ERR-FUN-COMANDO
AC0613         MOVE SQLCODE            TO  ERR-SQL-CODE
AC0613         MOVE '6220'             TO  ERR-LOCAL
AC0613         MOVE SPACES             TO  ERR-SEGM
AC0613         PERFORM 9999-ROTINA-ERRO
AC0613     END-IF.
AC0613
AC0613*---------------------------------------------------------------*
AC0613 6220-99-FIM.                    EXIT.
AC0613*---------------------------------------------------------------*

AC0613*---------------------------------------------------------------*
AC0613 6300-OPEN-CSR43-RUECB09H        SECTION.
AC0613*---------------------------------------------------------------*
AC0613
AC0613     EXEC SQL
AC0613       OPEN CSR43-RUECB09H
AC0613     END-EXEC.
AC0613
AC0613     IF  ( SQLCODE               NOT EQUAL ZEROS             )
AC0613     OR  ( SQLWARN0              EQUAL    'W'                )
AC0613         MOVE 'DB2'              TO  ERR-TIPO-ACESSO
AC0613         MOVE 'RUECB09H'         TO  ERR-DBD-TAB
AC0613         MOVE 'OPEN    '         TO  ERR-FUN-COMANDO
AC0613         MOVE SQLCODE            TO  ERR-SQL-CODE
AC0613         MOVE '6300'             TO  ERR-LOCAL
AC0613         MOVE SPACES             TO  ERR-SEGM
AC0613         PERFORM 9999-ROTINA-ERRO
AC0613     END-IF.
AC0613
AC0613*---------------------------------------------------------------*
AC0613 6300-99-FIM.                    EXIT.
AC0613*---------------------------------------------------------------*
AC0613
AC0613*---------------------------------------------------------------*
AC0613 6310-FETCH-CSR43-RUECB09H       SECTION.
AC0613*---------------------------------------------------------------*
AC0613
AC0613     INITIALIZE WRK-RUEC09HS-NULL
AC0613               REPLACING ALPHANUMERIC DATA BY SPACES.
AC0613
AC0613     INITIALIZE RUECB09H.
AC0613
AC0613     EXEC SQL
AC0613       FETCH CSR43-RUECB09H
AC0613       INTO
AC0613         :RUECB09H.CDEPDC,
AC0613         :RUECB09H.CCDULA-CREDT-RURAL,
AC0613         :RUECB09H.NPRODT-RURAL-TEMPR,
AC0613         :RUECB09H.CPRODT-RURAL
AC0613              :WRK-B09H-CPRODT-N,
AC0613         :RUECB09H.DULT-ATULZ
AC0613              :WRK-B09H-DULT-N,
AC0613         :RUECB09H.CUSUAR-RESP
AC0613              :WRK-B09H-CUSUAR-N
AC0613     END-EXEC.
AC0613
AC0613     IF  ( SQLCODE               NOT EQUAL ZEROS AND +100    )
AC0613     OR  ( SQLWARN0              EQUAL    'W'                )
AC0613         MOVE 'DB2'              TO  ERR-TIPO-ACESSO
AC0613         MOVE 'RUECB09H'         TO  ERR-DBD-TAB
AC0613         MOVE 'FETCH   '         TO  ERR-FUN-COMANDO
AC0613         MOVE SQLCODE            TO  ERR-SQL-CODE
AC0613         MOVE '6310'             TO  ERR-LOCAL
AC0613         MOVE SPACES             TO  ERR-SEGM
AC0613         PERFORM 9999-ROTINA-ERRO
AC0613     END-IF.
AC0613
AC0613     IF  SQLCODE                 EQUAL +100
AC0613         MOVE 'S'                TO  WRK-FIM-RUEC09HS
AC0613         GO                      TO  6310-99-FIM
AC0613     END-IF.
AC0613
AC0613     ADD 1                       TO ACU-LIDOS-RUEC09HS.
AC0613
AC0613     IF  WRK-B09H-CPRODT-N             NOT EQUAL ZEROS
AC0613         MOVE '?'             TO  WRK-B09H-CPRODT-NULL
AC0613     END-IF.
AC0613
AC0613     IF  WRK-B09H-DULT-N               NOT EQUAL ZEROS
AC0613         MOVE '?'             TO  WRK-B09H-DULT-NULL
AC0613     END-IF.
AC0613
AC0613     IF  WRK-B09H-CUSUAR-N             NOT EQUAL ZEROS
AC0613         MOVE '?'             TO  WRK-B09H-CUSUAR-NULL
AC0613     END-IF.
AC0613
AC0613     STRING  RUECB09H (01:13)
AC0613             WRK-B09H-CPRODT-NULL
AC0613             RUECB09H (14:10)
AC0613             WRK-B09H-DULT-NULL
AC0613             RUECB09H (24:09)
AC0613             WRK-B09H-CUSUAR-NULL
AC0613             DELIMITED BY SIZE
AC0613     INTO    FD-REG-RUEC09HS.
AC0613
AC0613     PERFORM 7450-GRAVAR-SAIDA-RUEC09HS.
AC0613
AC0613*---------------------------------------------------------------*
AC0613 6310-99-FIM.                    EXIT.
AC0613*---------------------------------------------------------------*
AC0613
AC0613*---------------------------------------------------------------*
AC0613 6320-CLOSE-CSR43-RUECB09H       SECTION.
AC0613*---------------------------------------------------------------*
AC0613
AC0613     EXEC SQL
AC0613       CLOSE CSR43-RUECB09H
AC0613     END-EXEC.
AC0613
AC0613     IF  ( SQLCODE               NOT EQUAL ZEROS             )
AC0613     OR  ( SQLWARN0              EQUAL    'W'                )
AC0613         MOVE 'DB2'              TO  ERR-TIPO-ACESSO
AC0613         MOVE 'RUECB09H'         TO  ERR-DBD-TAB
AC0613         MOVE 'CLOSE   '         TO  ERR-FUN-COMANDO
AC0613         MOVE SQLCODE            TO  ERR-SQL-CODE
AC0613         MOVE '6320'             TO  ERR-LOCAL
AC0613         MOVE SPACES             TO  ERR-SEGM
AC0613         PERFORM 9999-ROTINA-ERRO
AC0613     END-IF.
AC0613
AC0613*---------------------------------------------------------------*
AC0613 6320-99-FIM.                    EXIT.
AC0613*---------------------------------------------------------------*

AC0613*---------------------------------------------------------------*
AC0613 6400-OPEN-CSR44-RUECB09I        SECTION.
AC0613*---------------------------------------------------------------*
AC0613
AC0613     EXEC SQL
AC0613       OPEN CSR44-RUECB09I
AC0613     END-EXEC.
AC0613
AC0613     IF  ( SQLCODE               NOT EQUAL ZEROS             )
AC0613     OR  ( SQLWARN0              EQUAL    'W'                )
AC0613         MOVE 'DB2'              TO  ERR-TIPO-ACESSO
AC0613         MOVE 'RUECB09I'         TO  ERR-DBD-TAB
AC0613         MOVE 'OPEN    '         TO  ERR-FUN-COMANDO
AC0613         MOVE SQLCODE            TO  ERR-SQL-CODE
AC0613         MOVE '6400'             TO  ERR-LOCAL
AC0613         MOVE SPACES             TO  ERR-SEGM
AC0613         PERFORM 9999-ROTINA-ERRO
AC0613     END-IF.
AC0613
AC0613*---------------------------------------------------------------*
AC0613 6400-99-FIM.                    EXIT.
AC0613*---------------------------------------------------------------*
AC0613
AC0613*---------------------------------------------------------------*
AC0613 6410-FETCH-CSR44-RUECB09I       SECTION.
AC0613*---------------------------------------------------------------*
AC0613
AC0613     INITIALIZE WRK-RUEC09IS-NULL
AC0613               REPLACING ALPHANUMERIC DATA BY SPACES.
AC0613
AC0613     INITIALIZE RUECB09I.
AC0613
AC0613     EXEC SQL
AC0613       FETCH CSR44-RUECB09I
AC0613       INTO
AC0613         :RUECB09I.CCONTR-CREDT-RURAL,
AC0613         :RUECB09I.DVCTO-PCELA-CONTR,
AC0613         :RUECB09I.DSELEC-LASTR
AC0613              :WRK-B09I-DSELEC-N,
AC0613         :RUECB09I.NPCELA-PGTO-RURAL
AC0613              :WRK-B09I-NPCELA-N,
AC0613         :RUECB09I.CMODLD-JURO
AC0613              :WRK-B09I-CMODLD-N,
AC0613         :RUECB09I.VRJURO
AC0613              :WRK-B09I-VRJURO-N,
AC0613         :RUECB09I.VPCELA-ATULZ
AC0613              :WRK-B09I-VPCELA-N,
AC0613         :RUECB09I.VRESGT-LASTR
AC0613              :WRK-B09I-VRESGT-N,
AC0613         :RUECB09I.VSDO-ATULZ
AC0613              :WRK-B09I-VSDO-N,
AC0613         :RUECB09I.CSELEC-LASTR
AC0613              :WRK-B09I-CSELEC-N,
AC0613         :RUECB09I.DREG-LASTR
AC0613              :WRK-B09I-DREG-N,
AC0613         :RUECB09I.CREG-LASTR
AC0613              :WRK-B09I-CREG-N,
AC0613         :RUECB09I.CCMARA-REG-LASTR
AC0613              :WRK-B09I-CCMARA-N,
AC0613         :RUECB09I.NSERIE-REG-LASTR
AC0613              :WRK-B09I-NSERIE-N,
AC0613         :RUECB09I.CCONTR-CMARA
AC0613              :WRK-B09I-CCONTR-N,
AC0613         :RUECB09I.CSIT-LASTR
AC0613              :WRK-B09I-CSIT-N,
AC0613         :RUECB09I.CJUNC-DEPDC
AC0613              :WRK-B09I-CJUNC-N,
AC0613         :RUECB09I.CSECAO
AC0613              :WRK-B09I-CSECAO-N,
AC0613         :RUECB09I.CUSUAR-RESP-INCL
AC0613              :WRK-B09I-CUSUAR-N,
AC0613         :RUECB09I.RMOTVO-EXCL-LASTR
AC0613              :WRK-B09I-RMOTVO-N,
AC0613         :RUECB09I.CUSUAR-RESP-EXCL
AC0613              :WRK-B09I-RESP-N,
AC0613         :RUECB09I.DEXCL-LASTR
AC0613              :WRK-B09I-DEXCL-N
AC0613     END-EXEC.
AC0613
AC0613     IF  ( SQLCODE               NOT EQUAL ZEROS AND +100    )
AC0613     OR  ( SQLWARN0              EQUAL    'W'                )
AC0613         MOVE 'DB2'              TO  ERR-TIPO-ACESSO
AC0613         MOVE 'RUECB09I'         TO  ERR-DBD-TAB
AC0613         MOVE 'FETCH   '         TO  ERR-FUN-COMANDO
AC0613         MOVE SQLCODE            TO  ERR-SQL-CODE
AC0613         MOVE '6410'             TO  ERR-LOCAL
AC0613         MOVE SPACES             TO  ERR-SEGM
AC0613         PERFORM 9999-ROTINA-ERRO
AC0613     END-IF.
AC0613
AC0613     IF  SQLCODE                 EQUAL +100
AC0613         MOVE 'S'                TO  WRK-FIM-RUEC09IS
AC0613         GO                      TO  6410-99-FIM
AC0613     END-IF.
AC0613
AC0613     ADD 1                       TO ACU-LIDOS-RUEC09IS.
AC0613
AC0613     IF  WRK-B09I-DSELEC-N             NOT EQUAL ZEROS
AC0613         MOVE '?'             TO  WRK-B09I-DSELEC-NULL
AC0613     END-IF.
AC0613
AC0613     IF  WRK-B09I-NPCELA-N             NOT EQUAL ZEROS
AC0613         MOVE '?'             TO  WRK-B09I-NPCELA-NULL
AC0613     END-IF.
AC0613
AC0613     IF  WRK-B09I-CMODLD-N             NOT EQUAL ZEROS
AC0613         MOVE '?'             TO  WRK-B09I-CMODLD-NULL
AC0613     END-IF.
AC0613
AC0613     IF  WRK-B09I-VRJURO-N             NOT EQUAL ZEROS
AC0613         MOVE '?'             TO  WRK-B09I-VRJURO-NULL
AC0613     END-IF.
AC0613
AC0613     IF  WRK-B09I-VPCELA-N             NOT EQUAL ZEROS
AC0613         MOVE '?'             TO  WRK-B09I-VPCELA-NULL
AC0613     END-IF.
AC0613
AC0613     IF  WRK-B09I-VRESGT-N             NOT EQUAL ZEROS
AC0613         MOVE '?'             TO  WRK-B09I-VRESGT-NULL
AC0613     END-IF.
AC0613
AC0613     IF  WRK-B09I-VSDO-N               NOT EQUAL ZEROS
AC0613         MOVE '?'             TO  WRK-B09I-VSDO-NULL
AC0613     END-IF.
AC0613
AC0613     IF  WRK-B09I-CSELEC-N             NOT EQUAL ZEROS
AC0613         MOVE '?'             TO  WRK-B09I-CSELEC-NULL
AC0613     END-IF.
AC0613
AC0613     IF  WRK-B09I-DREG-N               NOT EQUAL ZEROS
AC0613         MOVE '?'             TO  WRK-B09I-DREG-NULL
AC0613     END-IF.
AC0613
AC0613     IF  WRK-B09I-CREG-N               NOT EQUAL ZEROS
AC0613         MOVE '?'             TO  WRK-B09I-CREG-NULL
AC0613     END-IF.
AC0613
AC0613     IF  WRK-B09I-CCMARA-N             NOT EQUAL ZEROS
AC0613         MOVE '?'             TO  WRK-B09I-CCMARA-NULL
AC0613     END-IF.
AC0613
AC0613     IF  WRK-B09I-NSERIE-N             NOT EQUAL ZEROS
AC0613         MOVE '?'             TO  WRK-B09I-NSERIE-NULL
AC0613     END-IF.
AC0613
AC0613     IF  WRK-B09I-CCONTR-N             NOT EQUAL ZEROS
AC0613         MOVE '?'             TO  WRK-B09I-CCONTR-NULL
AC0613     END-IF.
AC0613
AC0613     IF  WRK-B09I-CSIT-N               NOT EQUAL ZEROS
AC0613         MOVE '?'             TO  WRK-B09I-CSIT-NULL
AC0613     END-IF.
AC0613
AC0613     IF  WRK-B09I-CJUNC-N              NOT EQUAL ZEROS
AC0613         MOVE '?'             TO  WRK-B09I-CJUNC-NULL
AC0613     END-IF.
AC0613
AC0613     IF  WRK-B09I-CSECAO-N             NOT EQUAL ZEROS
AC0613         MOVE '?'             TO  WRK-B09I-CSECAO-NULL
AC0613     END-IF.
AC0613
AC0613     IF  WRK-B09I-CUSUAR-N             NOT EQUAL ZEROS
AC0613         MOVE '?'             TO  WRK-B09I-CUSUAR-NULL
AC0613     END-IF.
AC0613
AC0613     IF  WRK-B09I-RMOTVO-N             NOT EQUAL ZEROS
AC0613         MOVE '?'             TO  WRK-B09I-RMOTVO-NULL
AC0613     END-IF.
AC0613
AC0613     IF  WRK-B09I-RESP-N               NOT EQUAL ZEROS
AC0613         MOVE '?'             TO  WRK-B09I-RESP-NULL
AC0613     END-IF.
AC0613
AC0613     IF  WRK-B09I-DEXCL-N              NOT EQUAL ZEROS
AC0613         MOVE '?'             TO  WRK-B09I-DEXCL-NULL
AC0613     END-IF.
AC0613
AC0613     STRING  RUECB09I (01:25)
AC0613             WRK-B09I-DSELEC-NULL
AC0613             RUECB09I (26:02)
AC0613             WRK-B09I-NPCELA-NULL
AC0613             RUECB09I (28:01)
AC0613             WRK-B09I-CMODLD-NULL
AC0613             RUECB09I (29:08)
AC0613             WRK-B09I-VRJURO-NULL
AC0613             RUECB09I (37:08)
AC0613             WRK-B09I-VPCELA-NULL
AC0613             RUECB09I (45:08)
AC0613             WRK-B09I-VRESGT-NULL
AC0613             RUECB09I (53:08)
AC0613             WRK-B09I-VSDO-NULL
AC0613             RUECB09I (61:01)
AC0613             WRK-B09I-CSELEC-NULL
AC0613             RUECB09I (62:10)
AC0613             WRK-B09I-DREG-NULL
AC0613             RUECB09I (72:10)
AC0613             WRK-B09I-CREG-NULL
AC0613             RUECB09I (82:05)
AC0613             WRK-B09I-CCMARA-NULL
AC0613             RUECB09I (87:01)
AC0613             WRK-B09I-NSERIE-NULL
AC0613             RUECB09I (88:20)
AC0613             WRK-B09I-CCONTR-NULL
AC0613             RUECB09I (108:10)
AC0613             WRK-B09I-CSIT-NULL
AC0613             RUECB09I (118:03)
AC0613             WRK-B09I-CJUNC-NULL
AC0613             RUECB09I (121:02)
AC0613             WRK-B09I-CSECAO-NULL
AC0613             RUECB09I (123:09)
AC0613             WRK-B09I-CUSUAR-NULL
AC0613             RUECB09I (132:50)
AC0613             WRK-B09I-RMOTVO-NULL
AC0613             RUECB09I (182:09)
AC0613             WRK-B09I-RESP-NULL
AC0613             RUECB09I (191:10)
AC0613             WRK-B09I-DEXCL-NULL
AC0613             DELIMITED BY SIZE
AC0613     INTO    FD-REG-RUEC09IS.
AC0613
AC0613     PERFORM 7460-GRAVAR-SAIDA-RUEC09IS.
AC0613
AC0613*---------------------------------------------------------------*
AC0613 6410-99-FIM.                    EXIT.
AC0613*---------------------------------------------------------------*
AC0613
AC0613*---------------------------------------------------------------*
AC0613 6420-CLOSE-CSR44-RUECB09I       SECTION.
AC0613*---------------------------------------------------------------*
AC0613
AC0613     EXEC SQL
AC0613       CLOSE CSR44-RUECB09I
AC0613     END-EXEC.
AC0613
AC0613     IF  ( SQLCODE               NOT EQUAL ZEROS             )
AC0613     OR  ( SQLWARN0              EQUAL    'W'                )
AC0613         MOVE 'DB2'              TO  ERR-TIPO-ACESSO
AC0613         MOVE 'RUECB09I'         TO  ERR-DBD-TAB
AC0613         MOVE 'CLOSE   '         TO  ERR-FUN-COMANDO
AC0613         MOVE SQLCODE            TO  ERR-SQL-CODE
AC0613         MOVE '6420'             TO  ERR-LOCAL
AC0613         MOVE SPACES             TO  ERR-SEGM
AC0613         PERFORM 9999-ROTINA-ERRO
AC0613     END-IF.
AC0613
AC0613*---------------------------------------------------------------*
AC0613 6420-99-FIM.                    EXIT.
AC0613*---------------------------------------------------------------*

      *---------------------------------------------------------------*
       7030-GRAVAR-SAIDA-RUEC008S          SECTION.
      *---------------------------------------------------------------*

           MOVE   WRK-GRAVACAO     TO      WRK-OPERACAO.

           WRITE  FD-REG-RUEC008S.
           PERFORM 8030-TESTAR-FS-RUEC008S.

           ADD  1                  TO      ACU-GRAVS-RUEC008S
                                           ACU-GRAG-RUEC008S.

      *---------------------------------------------------------------*
       7030-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       7040-GRAVAR-SAIDA-RUEC009S          SECTION.
      *---------------------------------------------------------------*

           MOVE   WRK-GRAVACAO     TO      WRK-OPERACAO.

           WRITE  FD-REG-RUEC009S.
           PERFORM 8040-TESTAR-FS-RUEC009S.

           ADD  1                  TO      ACU-GRAVS-RUEC009S
                                           ACU-GRAG-RUEC009S.

      *---------------------------------------------------------------*
       7040-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       7050-GRAVAR-SAIDA-RUEC010S          SECTION.
      *---------------------------------------------------------------*

           MOVE   WRK-GRAVACAO     TO      WRK-OPERACAO.

           WRITE  FD-REG-RUEC010S.
           PERFORM 8050-TESTAR-FS-RUEC010S.

           ADD  1                  TO      ACU-GRAVS-RUEC010S
                                           ACU-GRAG-RUEC010S.

      *---------------------------------------------------------------*
       7050-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       7060-GRAVAR-SAIDA-RUEC011S          SECTION.
      *---------------------------------------------------------------*

           MOVE   WRK-GRAVACAO     TO      WRK-OPERACAO.

           WRITE  FD-REG-RUEC011S  FROM    RUECV011.
           PERFORM 8060-TESTAR-FS-RUEC011S.

           ADD  1                  TO      ACU-GRAVS-RUEC011S
                                           ACU-GRAG-RUEC011S.

      *---------------------------------------------------------------*
       7060-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       7070-GRAVAR-SAIDA-RUEC012S          SECTION.
      *---------------------------------------------------------------*

           MOVE   WRK-GRAVACAO     TO      WRK-OPERACAO.

           WRITE  FD-REG-RUEC012S  FROM    RUECV012.
           PERFORM 8070-TESTAR-FS-RUEC012S.

           ADD  1                  TO      ACU-GRAVS-RUEC012S
                                           ACU-GRAG-RUEC012S.

      *---------------------------------------------------------------*
       7070-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       7080-GRAVAR-SAIDA-RUEC014S          SECTION.
      *---------------------------------------------------------------*

           MOVE   WRK-GRAVACAO     TO      WRK-OPERACAO.

           WRITE  FD-REG-RUEC014S.
           PERFORM 8080-TESTAR-FS-RUEC014S.

           ADD  1                  TO      ACU-GRAVS-RUEC014S
                                           ACU-GRAG-RUEC014S.

      *---------------------------------------------------------------*
       7080-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       7090-GRAVAR-SAIDA-RUEC017S          SECTION.
      *---------------------------------------------------------------*

           MOVE   WRK-GRAVACAO     TO      WRK-OPERACAO.

           WRITE  FD-REG-RUEC017S.
           PERFORM 8090-TESTAR-FS-RUEC017S.

           ADD  1                  TO      ACU-GRAVS-RUEC017S
                                           ACU-GRAG-RUEC017S.

      *---------------------------------------------------------------*
       7090-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       7100-GRAVAR-SAIDA-RUEC018S          SECTION.
      *---------------------------------------------------------------*

           MOVE   WRK-GRAVACAO     TO      WRK-OPERACAO.

           WRITE  FD-REG-RUEC018S  FROM    RUECV018.
           PERFORM 8100-TESTAR-FS-RUEC018S.

           ADD  1                  TO      ACU-GRAVS-RUEC018S
                                           ACU-GRAG-RUEC018S.

      *---------------------------------------------------------------*
       7100-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       7110-GRAVAR-SAIDA-RUEC019S          SECTION.
      *---------------------------------------------------------------*

           MOVE   WRK-GRAVACAO     TO      WRK-OPERACAO.

           WRITE  FD-REG-RUEC019S  FROM    RUECV019.
           PERFORM 8110-TESTAR-FS-RUEC019S.

           ADD  1                  TO      ACU-GRAVS-RUEC019S
                                           ACU-GRAG-RUEC019S.

      *---------------------------------------------------------------*
       7110-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       7120-GRAVAR-SAIDA-RUEC022S          SECTION.
      *---------------------------------------------------------------*

           MOVE   WRK-GRAVACAO     TO      WRK-OPERACAO.

           WRITE  FD-REG-RUEC022S.
           PERFORM 8120-TESTAR-FS-RUEC022S.

           ADD  1                  TO      ACU-GRAVS-RUEC022S
                                           ACU-GRAG-RUEC022S.

      *---------------------------------------------------------------*
       7120-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       7130-GRAVAR-SAIDA-RUEC023S          SECTION.
      *---------------------------------------------------------------*

           MOVE   WRK-GRAVACAO     TO      WRK-OPERACAO.

           WRITE  FD-REG-RUEC023S  FROM    RUECV023.
           PERFORM 8130-TESTAR-FS-RUEC023S.

           ADD  1                  TO      ACU-GRAVS-RUEC023S
                                           ACU-GRAG-RUEC023S.

      *---------------------------------------------------------------*
       7130-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       7140-GRAVAR-SAIDA-RUEC024S          SECTION.
      *---------------------------------------------------------------*

           MOVE   WRK-GRAVACAO     TO      WRK-OPERACAO.

           WRITE  FD-REG-RUEC024S  FROM    RUECB024.
           PERFORM 8140-TESTAR-FS-RUEC024S.

           ADD  1                  TO      ACU-GRAVS-RUEC024S
                                           ACU-GRAG-RUEC024S.

      *---------------------------------------------------------------*
       7140-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       7150-GRAVAR-SAIDA-RUEC025S          SECTION.
      *---------------------------------------------------------------*

           MOVE   WRK-GRAVACAO     TO      WRK-OPERACAO.

           WRITE  FD-REG-RUEC025S.
           PERFORM 8150-TESTAR-FS-RUEC025S.

           ADD  1                  TO      ACU-GRAVS-RUEC025S
                                           ACU-GRAG-RUEC025S.

      *---------------------------------------------------------------*
       7150-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       7160-GRAVAR-SAIDA-RUEC026S          SECTION.
      *---------------------------------------------------------------*

           MOVE   WRK-GRAVACAO     TO      WRK-OPERACAO.

           WRITE  FD-REG-RUEC026S  FROM    RUECV026.
           PERFORM 8160-TESTAR-FS-RUEC026S.

           ADD  1                  TO      ACU-GRAVS-RUEC026S
                                           ACU-GRAG-RUEC026S.

      *---------------------------------------------------------------*
       7160-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       7170-GRAVAR-SAIDA-RUEC027S          SECTION.
      *---------------------------------------------------------------*

           MOVE   WRK-GRAVACAO     TO      WRK-OPERACAO.

           WRITE  FD-REG-RUEC027S  FROM    RUECB027.
           PERFORM 8170-TESTAR-FS-RUEC027S.

           ADD  1                  TO      ACU-GRAVS-RUEC027S
                                           ACU-GRAG-RUEC027S.

      *---------------------------------------------------------------*
       7170-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       7180-GRAVAR-SAIDA-RUEC028S          SECTION.
      *---------------------------------------------------------------*

           MOVE   WRK-GRAVACAO     TO      WRK-OPERACAO.

           WRITE  FD-REG-RUEC028S  FROM    RUECB028.
           PERFORM 8180-TESTAR-FS-RUEC028S.

           ADD  1                  TO      ACU-GRAVS-RUEC028S
                                           ACU-GRAG-RUEC028S.

      *---------------------------------------------------------------*
       7180-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       7190-GRAVAR-SAIDA-RUEC033S          SECTION.
      *---------------------------------------------------------------*

           MOVE   WRK-GRAVACAO     TO      WRK-OPERACAO.

           WRITE  FD-REG-RUEC033S  FROM    RUECB033.
           PERFORM 8190-TESTAR-FS-RUEC033S.

           ADD  1                  TO      ACU-GRAVS-RUEC033S
                                           ACU-GRAG-RUEC033S.

      *---------------------------------------------------------------*
       7190-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       7200-GRAVAR-SAIDA-RUEC034S          SECTION.
      *---------------------------------------------------------------*

           MOVE   WRK-GRAVACAO     TO      WRK-OPERACAO.

           WRITE  FD-REG-RUEC034S  FROM    RUECB034.
           PERFORM 8200-TESTAR-FS-RUEC034S.

           ADD  1                  TO      ACU-GRAVS-RUEC034S
                                           ACU-GRAG-RUEC034S.

      *---------------------------------------------------------------*
       7200-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       7210-GRAVAR-SAIDA-RUEC035S          SECTION.
      *---------------------------------------------------------------*

           MOVE   WRK-GRAVACAO     TO      WRK-OPERACAO.

           WRITE  FD-REG-RUEC035S.
           PERFORM 8210-TESTAR-FS-RUEC035S.

           ADD  1                  TO      ACU-GRAVS-RUEC035S
                                           ACU-GRAG-RUEC035S.

      *---------------------------------------------------------------*
       7210-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       7220-GRAVAR-SAIDA-RUEC036S          SECTION.
      *---------------------------------------------------------------*

           MOVE   WRK-GRAVACAO     TO      WRK-OPERACAO.

           WRITE  FD-REG-RUEC036S  FROM    RUECB036.
           PERFORM 8220-TESTAR-FS-RUEC036S.

           ADD  1                  TO      ACU-GRAVS-RUEC036S
                                           ACU-GRAG-RUEC036S.

      *---------------------------------------------------------------*
       7220-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       7230-GRAVAR-SAIDA-RUEC038S          SECTION.
      *---------------------------------------------------------------*

           MOVE   WRK-GRAVACAO     TO      WRK-OPERACAO.

           WRITE  FD-REG-RUEC038S  FROM    RUECB038.
           PERFORM 8230-TESTAR-FS-RUEC038S.

           ADD  1                  TO      ACU-GRAVS-RUEC038S
                                           ACU-GRAG-RUEC038S.

      *---------------------------------------------------------------*
       7230-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       7240-GRAVAR-SAIDA-RUEC040S          SECTION.
      *---------------------------------------------------------------*

           MOVE   WRK-GRAVACAO     TO      WRK-OPERACAO.

           WRITE  FD-REG-RUEC040S.
           PERFORM 8240-TESTAR-FS-RUEC040S.

           ADD  1                  TO      ACU-GRAVS-RUEC040S
                                           ACU-GRAG-RUEC040S.

      *---------------------------------------------------------------*
       7240-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       7250-GRAVAR-SAIDA-RUEC041S          SECTION.
      *---------------------------------------------------------------*

           MOVE   WRK-GRAVACAO     TO      WRK-OPERACAO.

           WRITE  FD-REG-RUEC041S.
           PERFORM 8250-TESTAR-FS-RUEC041S.

           ADD  1                  TO      ACU-GRAVS-RUEC041S
                                           ACU-GRAG-RUEC041S.

      *---------------------------------------------------------------*
       7250-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       7260-GRAVAR-SAIDA-RUEC044S          SECTION.
      *---------------------------------------------------------------*

           MOVE   WRK-GRAVACAO     TO      WRK-OPERACAO.

           WRITE  FD-REG-RUEC044S.
           PERFORM 8260-TESTAR-FS-RUEC044S.

           ADD  1                  TO      ACU-GRAVS-RUEC044S
                                           ACU-GRAG-RUEC044S.

      *---------------------------------------------------------------*
       7260-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       7270-GRAVAR-SAIDA-RUEC052S          SECTION.
      *---------------------------------------------------------------*

           MOVE   WRK-GRAVACAO     TO      WRK-OPERACAO.

           WRITE  FD-REG-RUEC052S.
           PERFORM 8270-TESTAR-FS-RUEC052S.

           ADD  1                  TO      ACU-GRAVS-RUEC052S
                                           ACU-GRAG-RUEC052S.

      *---------------------------------------------------------------*
       7270-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       7280-GRAVAR-SAIDA-RUEC053S          SECTION.
      *---------------------------------------------------------------*

           MOVE   WRK-GRAVACAO     TO      WRK-OPERACAO.

           WRITE  FD-REG-RUEC053S  FROM    RUECB053.
           PERFORM 8280-TESTAR-FS-RUEC053S.

           ADD  1                  TO      ACU-GRAVS-RUEC053S
                                           ACU-GRAG-RUEC053S.

      *---------------------------------------------------------------*
       7280-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       7290-GRAVAR-SAIDA-RUEC054S          SECTION.
      *---------------------------------------------------------------*

           MOVE   WRK-GRAVACAO     TO      WRK-OPERACAO.

           WRITE  FD-REG-RUEC054S  FROM    RUECB054.
           PERFORM 8290-TESTAR-FS-RUEC054S.

           ADD  1                  TO      ACU-GRAVS-RUEC054S
                                           ACU-GRAG-RUEC054S.

      *---------------------------------------------------------------*
       7290-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       7300-GRAVAR-SAIDA-RUEC064S          SECTION.
      *---------------------------------------------------------------*

           MOVE   WRK-GRAVACAO     TO      WRK-OPERACAO.

           WRITE  FD-REG-RUEC064S.
           PERFORM 8300-TESTAR-FS-RUEC064S.

           ADD  1                  TO      ACU-GRAVS-RUEC064S
                                           ACU-GRAG-RUEC064S.

      *---------------------------------------------------------------*
       7300-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       7310-GRAVAR-SAIDA-RUEC065S          SECTION.
      *---------------------------------------------------------------*

           MOVE   WRK-GRAVACAO     TO      WRK-OPERACAO.

           WRITE  FD-REG-RUEC065S.
           PERFORM 8310-TESTAR-FS-RUEC065S.

           ADD  1                  TO      ACU-GRAVS-RUEC065S
                                           ACU-GRAG-RUEC065S.

      *---------------------------------------------------------------*
       7310-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       7320-GRAVAR-SAIDA-RUEC066S          SECTION.
      *---------------------------------------------------------------*

           MOVE   WRK-GRAVACAO     TO      WRK-OPERACAO.

           WRITE  FD-REG-RUEC066S.
           PERFORM 8320-TESTAR-FS-RUEC066S.

           ADD  1                  TO      ACU-GRAVS-RUEC066S
                                           ACU-GRAG-RUEC066S.

      *---------------------------------------------------------------*
       7320-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       7330-GRAVAR-SAIDA-RUEC073S          SECTION.
      *---------------------------------------------------------------*

           MOVE   WRK-GRAVACAO     TO      WRK-OPERACAO.

           WRITE  FD-REG-RUEC073S  FROM    RUECB073.
           PERFORM 8330-TESTAR-FS-RUEC073S.

           ADD  1                  TO      ACU-GRAVS-RUEC073S
                                           ACU-GRAG-RUEC073S.

      *---------------------------------------------------------------*
       7330-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       7340-GRAVAR-SAIDA-RUEC074S          SECTION.
      *---------------------------------------------------------------*

           MOVE   WRK-GRAVACAO     TO      WRK-OPERACAO.

           WRITE  FD-REG-RUEC074S.
           PERFORM 8340-TESTAR-FS-RUEC074S.

           ADD  1                  TO      ACU-GRAVS-RUEC074S
                                           ACU-GRAG-RUEC074S.

      *---------------------------------------------------------------*
       7340-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       7350-GRAVAR-SAIDA-RUEC075S          SECTION.
      *---------------------------------------------------------------*

           MOVE   WRK-GRAVACAO     TO      WRK-OPERACAO.

           WRITE  FD-REG-RUEC075S.
           PERFORM 8350-TESTAR-FS-RUEC075S.

           ADD  1                  TO      ACU-GRAVS-RUEC075S
                                           ACU-GRAG-RUEC075S.

      *---------------------------------------------------------------*
       7350-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       7360-GRAVAR-SAIDA-RUEC096S          SECTION.
      *---------------------------------------------------------------*

           MOVE   WRK-GRAVACAO     TO      WRK-OPERACAO.

           WRITE  FD-REG-RUEC096S.
           PERFORM 8360-TESTAR-FS-RUEC096S.

           ADD  1                  TO      ACU-GRAVS-RUEC096S
                                           ACU-GRAG-RUEC096S.

      *---------------------------------------------------------------*
       7360-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       7370-GRAVAR-SAIDA-RUEC097S          SECTION.
      *---------------------------------------------------------------*

           MOVE   WRK-GRAVACAO     TO      WRK-OPERACAO.

           WRITE  FD-REG-RUEC097S.
           PERFORM 8370-TESTAR-FS-RUEC097S.

           ADD  1                  TO      ACU-GRAVS-RUEC097S
                                           ACU-GRAG-RUEC097S.

      *---------------------------------------------------------------*
       7370-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       7380-GRAVAR-SAIDA-RUEC099S          SECTION.
      *---------------------------------------------------------------*

           MOVE   WRK-GRAVACAO     TO      WRK-OPERACAO.

           WRITE  FD-REG-RUEC099S.
           PERFORM 8380-TESTAR-FS-RUEC099S.

           ADD  1                  TO      ACU-GRAVS-RUEC099S
                                           ACU-GRAG-RUEC099S.

      *---------------------------------------------------------------*
       7380-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       7390-GRAVAR-SAIDA-RUEC09AS          SECTION.
      *---------------------------------------------------------------*

           MOVE   WRK-GRAVACAO     TO      WRK-OPERACAO.

           WRITE  FD-REG-RUEC09AS.
           PERFORM 8390-TESTAR-FS-RUEC09AS.

           ADD  1                  TO      ACU-GRAVS-RUEC09AS
                                           ACU-GRAG-RUEC09AS.

      *---------------------------------------------------------------*
       7390-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       7400-GRAVAR-SAIDA-RUEC09BS          SECTION.
      *---------------------------------------------------------------*

           MOVE   WRK-GRAVACAO     TO      WRK-OPERACAO.

           WRITE  FD-REG-RUEC09BS.
           PERFORM 8400-TESTAR-FS-RUEC09BS.

           ADD  1                  TO      ACU-GRAVS-RUEC09BS
                                           ACU-GRAG-RUEC09BS.

      *---------------------------------------------------------------*
       7400-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       7410-GRAVAR-SAIDA-RUEC09DS          SECTION.
      *---------------------------------------------------------------*

           MOVE   WRK-GRAVACAO     TO      WRK-OPERACAO.

           WRITE  FD-REG-RUEC09DS.
           PERFORM 8410-TESTAR-FS-RUEC09DS.

           ADD  1                  TO      ACU-GRAVS-RUEC09DS
                                           ACU-GRAG-RUEC09DS.

      *---------------------------------------------------------------*
       7410-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       7420-GRAVAR-SAIDA-RUEC09ES          SECTION.
      *---------------------------------------------------------------*

           MOVE   WRK-GRAVACAO     TO      WRK-OPERACAO.

           WRITE  FD-REG-RUEC09ES.
           PERFORM 8420-TESTAR-FS-RUEC09ES.

           ADD  1                  TO      ACU-GRAVS-RUEC09ES
                                           ACU-GRAG-RUEC09ES.

      *---------------------------------------------------------------*
       7420-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       7430-GRAVAR-SAIDA-RUEC09FS          SECTION.
      *---------------------------------------------------------------*

           MOVE   WRK-GRAVACAO     TO      WRK-OPERACAO.

           WRITE  FD-REG-RUEC09FS.
           PERFORM 8430-TESTAR-FS-RUEC09FS.

           ADD  1                  TO      ACU-GRAVS-RUEC09FS
                                           ACU-GRAG-RUEC09FS.

      *---------------------------------------------------------------*
       7430-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       7440-GRAVAR-SAIDA-RUEC09GS          SECTION.
      *---------------------------------------------------------------*

           MOVE   WRK-GRAVACAO     TO      WRK-OPERACAO.

           WRITE  FD-REG-RUEC09GS.
           PERFORM 8440-TESTAR-FS-RUEC09GS.

           ADD  1                  TO      ACU-GRAVS-RUEC09GS
                                           ACU-GRAG-RUEC09GS.

      *---------------------------------------------------------------*
       7440-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       7450-GRAVAR-SAIDA-RUEC09HS          SECTION.
      *---------------------------------------------------------------*

           MOVE   WRK-GRAVACAO     TO      WRK-OPERACAO.

           WRITE  FD-REG-RUEC09HS.
           PERFORM 8450-TESTAR-FS-RUEC09HS.

           ADD  1                  TO      ACU-GRAVS-RUEC09HS
                                           ACU-GRAG-RUEC09HS.

      *---------------------------------------------------------------*
       7450-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       7460-GRAVAR-SAIDA-RUEC09IS          SECTION.
      *---------------------------------------------------------------*

           MOVE   WRK-GRAVACAO     TO      WRK-OPERACAO.

           WRITE  FD-REG-RUEC09IS.
           PERFORM 8460-TESTAR-FS-RUEC09IS.

           ADD  1                  TO      ACU-GRAVS-RUEC09IS
                                           ACU-GRAG-RUEC09IS.

      *---------------------------------------------------------------*
       7460-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       8000-TESTAR-FILE-STATUS         SECTION.
      *---------------------------------------------------------------*

           PERFORM 8010-TESTAR-FS-ARQDATAS.

           PERFORM 8020-TESTAR-FS-SELCDULA.

           PERFORM 8030-TESTAR-FS-RUEC008S.

           PERFORM 8040-TESTAR-FS-RUEC009S.

           PERFORM 8050-TESTAR-FS-RUEC010S.

           PERFORM 8060-TESTAR-FS-RUEC011S.

           PERFORM 8070-TESTAR-FS-RUEC012S.

           PERFORM 8080-TESTAR-FS-RUEC014S.

           PERFORM 8090-TESTAR-FS-RUEC017S.

           PERFORM 8100-TESTAR-FS-RUEC018S.

           PERFORM 8110-TESTAR-FS-RUEC019S.

           PERFORM 8120-TESTAR-FS-RUEC022S.

           PERFORM 8130-TESTAR-FS-RUEC023S.

           PERFORM 8140-TESTAR-FS-RUEC024S.

           PERFORM 8150-TESTAR-FS-RUEC025S.

           PERFORM 8160-TESTAR-FS-RUEC026S.

           PERFORM 8170-TESTAR-FS-RUEC027S.

           PERFORM 8180-TESTAR-FS-RUEC028S.

           PERFORM 8190-TESTAR-FS-RUEC033S.

           PERFORM 8200-TESTAR-FS-RUEC034S.

           PERFORM 8210-TESTAR-FS-RUEC035S.

           PERFORM 8220-TESTAR-FS-RUEC036S.

           PERFORM 8230-TESTAR-FS-RUEC038S.

           PERFORM 8240-TESTAR-FS-RUEC040S.

           PERFORM 8250-TESTAR-FS-RUEC041S.

           PERFORM 8260-TESTAR-FS-RUEC044S.

           PERFORM 8270-TESTAR-FS-RUEC052S.

           PERFORM 8280-TESTAR-FS-RUEC053S.

           PERFORM 8290-TESTAR-FS-RUEC054S.

           PERFORM 8300-TESTAR-FS-RUEC064S.

           PERFORM 8310-TESTAR-FS-RUEC065S.

           PERFORM 8320-TESTAR-FS-RUEC066S.

           PERFORM 8330-TESTAR-FS-RUEC073S.

           PERFORM 8340-TESTAR-FS-RUEC074S.

           PERFORM 8350-TESTAR-FS-RUEC075S.

           PERFORM 8360-TESTAR-FS-RUEC096S.

           PERFORM 8370-TESTAR-FS-RUEC097S.

           PERFORM 8380-TESTAR-FS-RUEC099S.

           PERFORM 8390-TESTAR-FS-RUEC09AS.

           PERFORM 8400-TESTAR-FS-RUEC09BS.

           PERFORM 8410-TESTAR-FS-RUEC09DS.

           PERFORM 8420-TESTAR-FS-RUEC09ES.

           PERFORM 8430-TESTAR-FS-RUEC09FS.

           PERFORM 8440-TESTAR-FS-RUEC09GS.

           PERFORM 8450-TESTAR-FS-RUEC09HS.

           PERFORM 8460-TESTAR-FS-RUEC09IS.


      *---------------------------------------------------------------*
       8000-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       8010-TESTAR-FS-ARQDATAS         SECTION.
      *---------------------------------------------------------------*

           IF  WRK-FS-ARQDATAS         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE 'ARQDATAS'         TO WRK-NOME-ARQ
               MOVE WRK-FS-ARQDATAS    TO WRK-FILE-STATUS
               MOVE WRK-MSG-FS-ERRO    TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       8010-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       8020-TESTAR-FS-SELCDULA         SECTION.
      *---------------------------------------------------------------*

           IF  WRK-FS-SELCDULA         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE 'SELCDULA'         TO WRK-NOME-ARQ
               MOVE WRK-FS-SELCDULA    TO WRK-FILE-STATUS
               MOVE WRK-MSG-FS-ERRO    TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       8020-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       8030-TESTAR-FS-RUEC008S         SECTION.
      *---------------------------------------------------------------*

           IF  WRK-FS-RUEC008S         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE 'RUEC008S'         TO WRK-NOME-ARQ
               MOVE WRK-FS-RUEC008S    TO WRK-FILE-STATUS
               MOVE WRK-MSG-FS-ERRO    TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       8030-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       8040-TESTAR-FS-RUEC009S         SECTION.
      *---------------------------------------------------------------*

           IF  WRK-FS-RUEC009S         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE 'RUEC009S'         TO WRK-NOME-ARQ
               MOVE WRK-FS-RUEC009S    TO WRK-FILE-STATUS
               MOVE WRK-MSG-FS-ERRO    TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       8040-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       8050-TESTAR-FS-RUEC010S         SECTION.
      *---------------------------------------------------------------*

           IF  WRK-FS-RUEC010S         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE 'RUEC010S'         TO WRK-NOME-ARQ
               MOVE WRK-FS-RUEC010S    TO WRK-FILE-STATUS
               MOVE WRK-MSG-FS-ERRO    TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       8050-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       8060-TESTAR-FS-RUEC011S         SECTION.
      *---------------------------------------------------------------*

           IF  WRK-FS-RUEC011S         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE 'RUEC011S'         TO WRK-NOME-ARQ
               MOVE WRK-FS-RUEC011S    TO WRK-FILE-STATUS
               MOVE WRK-MSG-FS-ERRO    TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       8060-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       8070-TESTAR-FS-RUEC012S         SECTION.
      *---------------------------------------------------------------*

           IF  WRK-FS-RUEC012S         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE 'RUEC012S'         TO WRK-NOME-ARQ
               MOVE WRK-FS-RUEC012S    TO WRK-FILE-STATUS
               MOVE WRK-MSG-FS-ERRO    TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       8070-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       8080-TESTAR-FS-RUEC014S         SECTION.
      *---------------------------------------------------------------*

           IF  WRK-FS-RUEC014S         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE 'RUEC014S'         TO WRK-NOME-ARQ
               MOVE WRK-FS-RUEC014S    TO WRK-FILE-STATUS
               MOVE WRK-MSG-FS-ERRO    TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       8080-99-FIM.                    EXIT.
      *---------------------------------------------------------------*


      *---------------------------------------------------------------*
       8090-TESTAR-FS-RUEC017S         SECTION.
      *---------------------------------------------------------------*

           IF  WRK-FS-RUEC017S         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE 'RUEC017S'         TO WRK-NOME-ARQ
               MOVE WRK-FS-RUEC017S    TO WRK-FILE-STATUS
               MOVE WRK-MSG-FS-ERRO    TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       8090-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       8100-TESTAR-FS-RUEC018S         SECTION.
      *---------------------------------------------------------------*

           IF  WRK-FS-RUEC018S         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE 'RUEC018S'         TO WRK-NOME-ARQ
               MOVE WRK-FS-RUEC018S    TO WRK-FILE-STATUS
               MOVE WRK-MSG-FS-ERRO    TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       8100-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       8110-TESTAR-FS-RUEC019S         SECTION.
      *---------------------------------------------------------------*

           IF  WRK-FS-RUEC019S         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE 'RUEC019S'         TO WRK-NOME-ARQ
               MOVE WRK-FS-RUEC019S    TO WRK-FILE-STATUS
               MOVE WRK-MSG-FS-ERRO    TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       8110-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       8120-TESTAR-FS-RUEC022S         SECTION.
      *---------------------------------------------------------------*

           IF  WRK-FS-RUEC022S         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE 'RUEC022S'         TO WRK-NOME-ARQ
               MOVE WRK-FS-RUEC022S    TO WRK-FILE-STATUS
               MOVE WRK-MSG-FS-ERRO    TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       8120-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       8130-TESTAR-FS-RUEC023S         SECTION.
      *---------------------------------------------------------------*

           IF  WRK-FS-RUEC023S         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE 'RUEC023S'         TO WRK-NOME-ARQ
               MOVE WRK-FS-RUEC023S    TO WRK-FILE-STATUS
               MOVE WRK-MSG-FS-ERRO    TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       8130-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       8140-TESTAR-FS-RUEC024S         SECTION.
      *---------------------------------------------------------------*

           IF  WRK-FS-RUEC024S         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE 'RUEC024S'         TO WRK-NOME-ARQ
               MOVE WRK-FS-RUEC024S    TO WRK-FILE-STATUS
               MOVE WRK-MSG-FS-ERRO    TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       8140-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       8150-TESTAR-FS-RUEC025S         SECTION.
      *---------------------------------------------------------------*

           IF  WRK-FS-RUEC025S         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE 'RUEC025S'         TO WRK-NOME-ARQ
               MOVE WRK-FS-RUEC025S    TO WRK-FILE-STATUS
               MOVE WRK-MSG-FS-ERRO    TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       8150-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       8160-TESTAR-FS-RUEC026S         SECTION.
      *---------------------------------------------------------------*

           IF  WRK-FS-RUEC026S         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE 'RUEC026S'         TO WRK-NOME-ARQ
               MOVE WRK-FS-RUEC026S    TO WRK-FILE-STATUS
               MOVE WRK-MSG-FS-ERRO    TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       8160-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       8170-TESTAR-FS-RUEC027S         SECTION.
      *---------------------------------------------------------------*

           IF  WRK-FS-RUEC027S         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE 'RUEC027S'         TO WRK-NOME-ARQ
               MOVE WRK-FS-RUEC027S    TO WRK-FILE-STATUS
               MOVE WRK-MSG-FS-ERRO    TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       8170-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       8180-TESTAR-FS-RUEC028S         SECTION.
      *---------------------------------------------------------------*

           IF  WRK-FS-RUEC028S         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE 'RUEC028S'         TO WRK-NOME-ARQ
               MOVE WRK-FS-RUEC028S    TO WRK-FILE-STATUS
               MOVE WRK-MSG-FS-ERRO    TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       8180-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       8190-TESTAR-FS-RUEC033S         SECTION.
      *---------------------------------------------------------------*

           IF  WRK-FS-RUEC033S         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE 'RUEC033S'         TO WRK-NOME-ARQ
               MOVE WRK-FS-RUEC033S    TO WRK-FILE-STATUS
               MOVE WRK-MSG-FS-ERRO    TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       8190-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       8200-TESTAR-FS-RUEC034S         SECTION.
      *---------------------------------------------------------------*

           IF  WRK-FS-RUEC034S         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE 'RUEC034S'         TO WRK-NOME-ARQ
               MOVE WRK-FS-RUEC034S    TO WRK-FILE-STATUS
               MOVE WRK-MSG-FS-ERRO    TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       8200-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       8210-TESTAR-FS-RUEC035S         SECTION.
      *---------------------------------------------------------------*

           IF  WRK-FS-RUEC035S         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE 'RUEC035S'         TO WRK-NOME-ARQ
               MOVE WRK-FS-RUEC035S    TO WRK-FILE-STATUS
               MOVE WRK-MSG-FS-ERRO    TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       8210-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       8220-TESTAR-FS-RUEC036S         SECTION.
      *---------------------------------------------------------------*

           IF  WRK-FS-RUEC036S         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE 'RUEC036S'         TO WRK-NOME-ARQ
               MOVE WRK-FS-RUEC036S    TO WRK-FILE-STATUS
               MOVE WRK-MSG-FS-ERRO    TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       8220-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       8230-TESTAR-FS-RUEC038S         SECTION.
      *---------------------------------------------------------------*

           IF  WRK-FS-RUEC038S         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE 'RUEC038S'         TO WRK-NOME-ARQ
               MOVE WRK-FS-RUEC038S    TO WRK-FILE-STATUS
               MOVE WRK-MSG-FS-ERRO    TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       8230-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       8240-TESTAR-FS-RUEC040S         SECTION.
      *---------------------------------------------------------------*

           IF  WRK-FS-RUEC040S         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE 'RUEC040S'         TO WRK-NOME-ARQ
               MOVE WRK-FS-RUEC040S    TO WRK-FILE-STATUS
               MOVE WRK-MSG-FS-ERRO    TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       8240-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       8250-TESTAR-FS-RUEC041S         SECTION.
      *---------------------------------------------------------------*

           IF  WRK-FS-RUEC041S         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE 'RUEC041S'         TO WRK-NOME-ARQ
               MOVE WRK-FS-RUEC041S    TO WRK-FILE-STATUS
               MOVE WRK-MSG-FS-ERRO    TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       8250-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       8260-TESTAR-FS-RUEC044S         SECTION.
      *---------------------------------------------------------------*

           IF  WRK-FS-RUEC044S         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE 'RUEC044S'         TO WRK-NOME-ARQ
               MOVE WRK-FS-RUEC044S    TO WRK-FILE-STATUS
               MOVE WRK-MSG-FS-ERRO    TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       8260-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       8270-TESTAR-FS-RUEC052S         SECTION.
      *---------------------------------------------------------------*

           IF  WRK-FS-RUEC052S         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE 'RUEC052S'         TO WRK-NOME-ARQ
               MOVE WRK-FS-RUEC052S    TO WRK-FILE-STATUS
               MOVE WRK-MSG-FS-ERRO    TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       8270-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       8280-TESTAR-FS-RUEC053S         SECTION.
      *---------------------------------------------------------------*

           IF  WRK-FS-RUEC053S         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE 'RUEC053S'         TO WRK-NOME-ARQ
               MOVE WRK-FS-RUEC053S    TO WRK-FILE-STATUS
               MOVE WRK-MSG-FS-ERRO    TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       8280-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       8290-TESTAR-FS-RUEC054S         SECTION.
      *---------------------------------------------------------------*

           IF  WRK-FS-RUEC054S         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE 'RUEC054S'         TO WRK-NOME-ARQ
               MOVE WRK-FS-RUEC054S    TO WRK-FILE-STATUS
               MOVE WRK-MSG-FS-ERRO    TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       8290-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       8300-TESTAR-FS-RUEC064S         SECTION.
      *---------------------------------------------------------------*

           IF  WRK-FS-RUEC064S         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE 'RUEC064S'         TO WRK-NOME-ARQ
               MOVE WRK-FS-RUEC064S    TO WRK-FILE-STATUS
               MOVE WRK-MSG-FS-ERRO    TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       8300-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       8310-TESTAR-FS-RUEC065S         SECTION.
      *---------------------------------------------------------------*

           IF  WRK-FS-RUEC065S         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE 'RUEC065S'         TO WRK-NOME-ARQ
               MOVE WRK-FS-RUEC065S    TO WRK-FILE-STATUS
               MOVE WRK-MSG-FS-ERRO    TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       8310-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       8320-TESTAR-FS-RUEC066S         SECTION.
      *---------------------------------------------------------------*

           IF  WRK-FS-RUEC066S         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE 'RUEC066S'         TO WRK-NOME-ARQ
               MOVE WRK-FS-RUEC066S    TO WRK-FILE-STATUS
               MOVE WRK-MSG-FS-ERRO    TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       8320-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       8330-TESTAR-FS-RUEC073S         SECTION.
      *---------------------------------------------------------------*

           IF  WRK-FS-RUEC073S         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE 'RUEC073S'         TO WRK-NOME-ARQ
               MOVE WRK-FS-RUEC073S    TO WRK-FILE-STATUS
               MOVE WRK-MSG-FS-ERRO    TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       8330-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       8340-TESTAR-FS-RUEC074S         SECTION.
      *---------------------------------------------------------------*

           IF  WRK-FS-RUEC074S         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE 'RUEC074S'         TO WRK-NOME-ARQ
               MOVE WRK-FS-RUEC074S    TO WRK-FILE-STATUS
               MOVE WRK-MSG-FS-ERRO    TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       8340-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       8350-TESTAR-FS-RUEC075S         SECTION.
      *---------------------------------------------------------------*

           IF  WRK-FS-RUEC075S         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE 'RUEC075S'         TO WRK-NOME-ARQ
               MOVE WRK-FS-RUEC075S    TO WRK-FILE-STATUS
               MOVE WRK-MSG-FS-ERRO    TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       8350-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       8360-TESTAR-FS-RUEC096S         SECTION.
      *---------------------------------------------------------------*

           IF  WRK-FS-RUEC096S         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE 'RUEC096S'         TO WRK-NOME-ARQ
               MOVE WRK-FS-RUEC096S    TO WRK-FILE-STATUS
               MOVE WRK-MSG-FS-ERRO    TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       8360-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       8370-TESTAR-FS-RUEC097S         SECTION.
      *---------------------------------------------------------------*

           IF  WRK-FS-RUEC097S         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE 'RUEC097S'         TO WRK-NOME-ARQ
               MOVE WRK-FS-RUEC097S    TO WRK-FILE-STATUS
               MOVE WRK-MSG-FS-ERRO    TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       8370-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       8380-TESTAR-FS-RUEC099S         SECTION.
      *---------------------------------------------------------------*

           IF  WRK-FS-RUEC099S         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE 'RUEC099S'         TO WRK-NOME-ARQ
               MOVE WRK-FS-RUEC099S    TO WRK-FILE-STATUS
               MOVE WRK-MSG-FS-ERRO    TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       8380-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       8390-TESTAR-FS-RUEC09AS         SECTION.
      *---------------------------------------------------------------*

           IF  WRK-FS-RUEC09AS         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE 'RUEC09AS'         TO WRK-NOME-ARQ
               MOVE WRK-FS-RUEC09AS    TO WRK-FILE-STATUS
               MOVE WRK-MSG-FS-ERRO    TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       8390-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       8400-TESTAR-FS-RUEC09BS         SECTION.
      *---------------------------------------------------------------*

           IF  WRK-FS-RUEC09BS         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE 'RUEC09BS'         TO WRK-NOME-ARQ
               MOVE WRK-FS-RUEC09BS    TO WRK-FILE-STATUS
               MOVE WRK-MSG-FS-ERRO    TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       8400-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       8410-TESTAR-FS-RUEC09DS         SECTION.
      *---------------------------------------------------------------*

           IF  WRK-FS-RUEC09DS         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE 'RUEC09DS'         TO WRK-NOME-ARQ
               MOVE WRK-FS-RUEC09DS    TO WRK-FILE-STATUS
               MOVE WRK-MSG-FS-ERRO    TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       8410-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       8420-TESTAR-FS-RUEC09ES         SECTION.
      *---------------------------------------------------------------*

           IF  WRK-FS-RUEC09ES         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE 'RUEC09ES'         TO WRK-NOME-ARQ
               MOVE WRK-FS-RUEC09ES    TO WRK-FILE-STATUS
               MOVE WRK-MSG-FS-ERRO    TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       8420-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       8430-TESTAR-FS-RUEC09FS         SECTION.
      *---------------------------------------------------------------*

           IF  WRK-FS-RUEC09FS         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE 'RUEC09FS'         TO WRK-NOME-ARQ
               MOVE WRK-FS-RUEC09FS    TO WRK-FILE-STATUS
               MOVE WRK-MSG-FS-ERRO    TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       8430-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       8440-TESTAR-FS-RUEC09GS         SECTION.
      *---------------------------------------------------------------*

           IF  WRK-FS-RUEC09GS         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE 'RUEC09GS'         TO WRK-NOME-ARQ
               MOVE WRK-FS-RUEC09GS    TO WRK-FILE-STATUS
               MOVE WRK-MSG-FS-ERRO    TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       8440-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       8450-TESTAR-FS-RUEC09HS         SECTION.
      *---------------------------------------------------------------*

           IF  WRK-FS-RUEC09HS         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE 'RUEC09HS'         TO WRK-NOME-ARQ
               MOVE WRK-FS-RUEC09HS    TO WRK-FILE-STATUS
               MOVE WRK-MSG-FS-ERRO    TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       8450-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       8460-TESTAR-FS-RUEC09IS         SECTION.
      *---------------------------------------------------------------*

           IF  WRK-FS-RUEC09IS         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE 'RUEC09IS'         TO WRK-NOME-ARQ
               MOVE WRK-FS-RUEC09IS    TO WRK-FILE-STATUS
               MOVE WRK-MSG-FS-ERRO    TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       8460-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       9000-FINALIZAR                  SECTION.
      *---------------------------------------------------------------*

           PERFORM 2050-TOTAL-AGENCIA-LIDA.
           PERFORM 9100-DISPLAY-TOTAIS.

           CLOSE   ARQDATAS
                   SELCDULA
                   RUEC008S
                   RUEC009S
                   RUEC010S
                   RUEC011S
                   RUEC012S
                   RUEC014S
                   RUEC017S
                   RUEC018S
                   RUEC019S
                   RUEC022S
                   RUEC023S
                   RUEC024S
                   RUEC025S
                   RUEC026S
                   RUEC027S
                   RUEC028S
                   RUEC033S
                   RUEC034S
                   RUEC035S
                   RUEC036S
                   RUEC038S
                   RUEC040S
                   RUEC041S
                   RUEC044S
                   RUEC052S
                   RUEC053S
                   RUEC054S
                   RUEC064S
                   RUEC065S
                   RUEC066S
                   RUEC073S
                   RUEC074S
                   RUEC075S
                   RUEC096S
                   RUEC097S
                   RUEC099S
                   RUEC09AS
                   RUEC09BS
                   RUEC09DS
                   RUEC09ES
                   RUEC09FS
                   RUEC09GS
                   RUEC09HS
                   RUEC09IS.

           MOVE WRK-FECHAMENTO          TO WRK-OPERACAO.
           PERFORM 8000-TESTAR-FILE-STATUS.

      *---------------------------------------------------------------*
       9000-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       9100-DISPLAY-TOTAIS             SECTION.
      *---------------------------------------------------------------*

           STRING WRK-TIMESTAMP(07:02) '/'
                  WRK-TIMESTAMP(05:02) '/'
                  WRK-TIMESTAMP(01:04)
           DELIMITED BY SIZE           INTO WRK-DATAPROC.

           STRING WRK-TIMESTAMP(09:02) ':'
                  WRK-TIMESTAMP(11:02) ':'
                  WRK-TIMESTAMP(13:02)
           DELIMITED BY SIZE           INTO WRK-HORAPROC.

           CALL  'BRAD0160'  USING  WRK-JOBNOME WRK-VALORFAC.

           MOVE 'RUEC1450'             TO  WRK-PROGRAMA.
           MOVE WRK-JOBNOME            TO  WRK-JOBNAME.

           MOVE N090DTIN               TO  WRK-DATA-ISO-N
           MOVE WRK-DIA-ISO            TO  WRK-DIA-EDIT
           MOVE WRK-MES-ISO            TO  WRK-MES-EDIT
           MOVE WRK-ANO-ISO            TO  WRK-ANO-EDIT
           MOVE WRK-DATA-EDIT          TO  WRK-DATAMOV.

           DISPLAY WRK-DISP1.
           DISPLAY WRK-DISP2.
           DISPLAY WRK-DISP3.
           DISPLAY WRK-DISP4.
           DISPLAY WRK-DISP5.
           DISPLAY WRK-DISP6.
           DISPLAY WRK-DISP3.
           DISPLAY WRK-DISP7.
           DISPLAY WRK-DISP8.


           MOVE 'SELCDULA'                     TO WRK-DDNAME.
           MOVE 'I'                            TO WRK-I-O.
           MOVE 'TOTAL LIDOS SELCDULA       '  TO WRK-DESCARQ.
           MOVE ACU-LIDOS-SELCDULA             TO WRK-QTDEARQ.
           DISPLAY WRK-DISP9.

           MOVE 'ARQDATAS'                     TO WRK-DDNAME.
           MOVE 'I'                            TO WRK-I-O.
           MOVE 'ARQUIVO DE DATAS           '  TO WRK-DESCARQ.
           MOVE ACU-LIDOS-ARQDATAS             TO WRK-QTDEARQ.
           DISPLAY WRK-DISP9.

           MOVE 'RUECB008'                     TO WRK-DDNAME.
           MOVE 'I'                            TO WRK-I-O.
           MOVE 'TOTAL LIDOS RUECB008'         TO WRK-DESCARQ.
           MOVE ACU-LIDOS-RUEC008S             TO WRK-QTDEARQ.
           DISPLAY WRK-DISP9.

           MOVE 'RUECB008'                     TO WRK-DDNAME.
           MOVE 'O'                            TO WRK-I-O.
           MOVE 'REGISTROS SELECIONADOS'       TO WRK-DESCARQ.
           MOVE ACU-GRAVS-RUEC008S             TO WRK-QTDEARQ.
           DISPLAY WRK-DISP9.

           MOVE 'RUECB009'                     TO WRK-DDNAME.
           MOVE 'I'                            TO WRK-I-O.
           MOVE 'TOTAL LIDOS RUECB009'         TO WRK-DESCARQ.
           MOVE ACU-LIDOS-RUEC009S             TO WRK-QTDEARQ.
           DISPLAY WRK-DISP9.

           MOVE 'RUECB009'                     TO WRK-DDNAME.
           MOVE 'O'                            TO WRK-I-O.
           MOVE 'REGISTROS SELECIONADOS'       TO WRK-DESCARQ.
           MOVE ACU-GRAVS-RUEC009S             TO WRK-QTDEARQ.
           DISPLAY WRK-DISP9.

           MOVE 'RUECV010'                     TO WRK-DDNAME.
           MOVE 'I'                            TO WRK-I-O.
           MOVE 'TOTAL LIDOS RUECV010'         TO WRK-DESCARQ.
           MOVE ACU-LIDOS-RUEC010S             TO WRK-QTDEARQ.
           DISPLAY WRK-DISP9.

           MOVE 'RUECV010'                     TO WRK-DDNAME.
           MOVE 'O'                            TO WRK-I-O.
           MOVE 'REGISTROS SELECIONADOS'       TO WRK-DESCARQ.
           MOVE ACU-GRAVS-RUEC010S             TO WRK-QTDEARQ.
           DISPLAY WRK-DISP9.

           MOVE 'RUECV011'                     TO WRK-DDNAME.
           MOVE 'I'                            TO WRK-I-O.
           MOVE 'TOTAL LIDOS RUECV011'         TO WRK-DESCARQ.
           MOVE ACU-LIDOS-RUEC011S             TO WRK-QTDEARQ.
           DISPLAY WRK-DISP9.

           MOVE 'RUECV011'                     TO WRK-DDNAME.
           MOVE 'O'                            TO WRK-I-O.
           MOVE 'REGISTROS SELECIONADOS'       TO WRK-DESCARQ.
           MOVE ACU-GRAVS-RUEC011S             TO WRK-QTDEARQ.
           DISPLAY WRK-DISP9.

           MOVE 'RUECV012'                     TO WRK-DDNAME.
           MOVE 'I'                            TO WRK-I-O.
           MOVE 'TOTAL LIDOS RUECV012'         TO WRK-DESCARQ.
           MOVE ACU-LIDOS-RUEC012S             TO WRK-QTDEARQ.
           DISPLAY WRK-DISP9.

           MOVE 'RUECV012'                     TO WRK-DDNAME.
           MOVE 'O'                            TO WRK-I-O.
           MOVE 'REGISTROS SELECIONADOS'       TO WRK-DESCARQ.
           MOVE ACU-GRAVS-RUEC012S             TO WRK-QTDEARQ.
           DISPLAY WRK-DISP9.

           MOVE 'RUECB014'                     TO WRK-DDNAME.
           MOVE 'I'                            TO WRK-I-O.
           MOVE 'TOTAL LIDOS RUECB014'         TO WRK-DESCARQ.
           MOVE ACU-LIDOS-RUEC014S             TO WRK-QTDEARQ.
           DISPLAY WRK-DISP9.

           MOVE 'RUECB014'                     TO WRK-DDNAME.
           MOVE 'O'                            TO WRK-I-O.
           MOVE 'REGISTROS SELECIONADOS'       TO WRK-DESCARQ.
           MOVE ACU-GRAVS-RUEC014S             TO WRK-QTDEARQ.
           DISPLAY WRK-DISP9.

           MOVE 'RUECB017'                     TO WRK-DDNAME.
           MOVE 'I'                            TO WRK-I-O.
           MOVE 'TOTAL LIDOS RUECB017'         TO WRK-DESCARQ.
           MOVE ACU-LIDOS-RUEC017S             TO WRK-QTDEARQ.
           DISPLAY WRK-DISP9.

           MOVE 'RUECB017'                     TO WRK-DDNAME.
           MOVE 'O'                            TO WRK-I-O.
           MOVE 'REGISTROS SELECIONADOS'       TO WRK-DESCARQ.
           MOVE ACU-GRAVS-RUEC017S             TO WRK-QTDEARQ.
           DISPLAY WRK-DISP9.

           MOVE 'RUECV018'                     TO WRK-DDNAME.
           MOVE 'I'                            TO WRK-I-O.
           MOVE 'TOTAL LIDOS RUECV018'         TO WRK-DESCARQ.
           MOVE ACU-LIDOS-RUEC018S             TO WRK-QTDEARQ.
           DISPLAY WRK-DISP9.

           MOVE 'RUECV018'                     TO WRK-DDNAME.
           MOVE 'O'                            TO WRK-I-O.
           MOVE 'REGISTROS SELECIONADOS'       TO WRK-DESCARQ.
           MOVE ACU-GRAVS-RUEC018S             TO WRK-QTDEARQ.
           DISPLAY WRK-DISP9.

           MOVE 'RUECV019'                     TO WRK-DDNAME.
           MOVE 'I'                            TO WRK-I-O.
           MOVE 'TOTAL LIDOS RUECV019'         TO WRK-DESCARQ.
           MOVE ACU-LIDOS-RUEC019S             TO WRK-QTDEARQ.
           DISPLAY WRK-DISP9.

           MOVE 'RUECV019'                     TO WRK-DDNAME.
           MOVE 'O'                            TO WRK-I-O.
           MOVE 'REGISTROS SELECIONADOS'       TO WRK-DESCARQ.
           MOVE ACU-GRAVS-RUEC019S             TO WRK-QTDEARQ.
           DISPLAY WRK-DISP9.

           MOVE 'RUECV022'                     TO WRK-DDNAME.
           MOVE 'I'                            TO WRK-I-O.
           MOVE 'TOTAL LIDOS RUECV022'         TO WRK-DESCARQ.
           MOVE ACU-LIDOS-RUEC022S             TO WRK-QTDEARQ.
           DISPLAY WRK-DISP9.

           MOVE 'RUECV022'                     TO WRK-DDNAME.
           MOVE 'O'                            TO WRK-I-O.
           MOVE 'REGISTROS SELECIONADOS'       TO WRK-DESCARQ.
           MOVE ACU-GRAVS-RUEC022S             TO WRK-QTDEARQ.
           DISPLAY WRK-DISP9.

           MOVE 'RUECV023'                     TO WRK-DDNAME.
           MOVE 'I'                            TO WRK-I-O.
           MOVE 'TOTAL LIDOS RUECV023'         TO WRK-DESCARQ.
           MOVE ACU-LIDOS-RUEC023S             TO WRK-QTDEARQ.
           DISPLAY WRK-DISP9.

           MOVE 'RUECV023'                     TO WRK-DDNAME.
           MOVE 'O'                            TO WRK-I-O.
           MOVE 'REGISTROS SELECIONADOS'       TO WRK-DESCARQ.
           MOVE ACU-GRAVS-RUEC023S             TO WRK-QTDEARQ.
           DISPLAY WRK-DISP9.

           MOVE 'RUECB024'                     TO WRK-DDNAME.
           MOVE 'I'                            TO WRK-I-O.
           MOVE 'TOTAL LIDOS RUECB024'         TO WRK-DESCARQ.
           MOVE ACU-LIDOS-RUEC024S             TO WRK-QTDEARQ.
           DISPLAY WRK-DISP9.

           MOVE 'RUECB024'                     TO WRK-DDNAME.
           MOVE 'O'                            TO WRK-I-O.
           MOVE 'REGISTROS SELECIONADOS'       TO WRK-DESCARQ.
           MOVE ACU-GRAVS-RUEC024S             TO WRK-QTDEARQ.
           DISPLAY WRK-DISP9.

           MOVE 'RUECV025'                     TO WRK-DDNAME.
           MOVE 'I'                            TO WRK-I-O.
           MOVE 'TOTAL LIDOS RUECV025'         TO WRK-DESCARQ.
           MOVE ACU-LIDOS-RUEC025S             TO WRK-QTDEARQ.
           DISPLAY WRK-DISP9.

           MOVE 'RUECV025'                     TO WRK-DDNAME.
           MOVE 'O'                            TO WRK-I-O.
           MOVE 'REGISTROS SELECIONADOS'       TO WRK-DESCARQ.
           MOVE ACU-GRAVS-RUEC025S             TO WRK-QTDEARQ.
           DISPLAY WRK-DISP9.

           MOVE 'RUECV026'                     TO WRK-DDNAME.
           MOVE 'I'                            TO WRK-I-O.
           MOVE 'TOTAL LIDOS RUECV026'         TO WRK-DESCARQ.
           MOVE ACU-LIDOS-RUEC026S             TO WRK-QTDEARQ.
           DISPLAY WRK-DISP9.

           MOVE 'RUECV026'                     TO WRK-DDNAME.
           MOVE 'O'                            TO WRK-I-O.
           MOVE 'REGISTROS SELECIONADOS'       TO WRK-DESCARQ.
           MOVE ACU-GRAVS-RUEC026S             TO WRK-QTDEARQ.
           DISPLAY WRK-DISP9.

           MOVE 'RUECB027'                     TO WRK-DDNAME.
           MOVE 'I'                            TO WRK-I-O.
           MOVE 'TOTAL LIDOS RUECB027'         TO WRK-DESCARQ.
           MOVE ACU-LIDOS-RUEC027S             TO WRK-QTDEARQ.
           DISPLAY WRK-DISP9.

           MOVE 'RUECV027'                     TO WRK-DDNAME.
           MOVE 'O'                            TO WRK-I-O.
           MOVE 'REGISTROS SELECIONADOS'       TO WRK-DESCARQ.
           MOVE ACU-GRAVS-RUEC027S             TO WRK-QTDEARQ.
           DISPLAY WRK-DISP9.

           MOVE 'RUECB028'                     TO WRK-DDNAME.
           MOVE 'I'                            TO WRK-I-O.
           MOVE 'TOTAL LIDOS RUECB028'         TO WRK-DESCARQ.
           MOVE ACU-LIDOS-RUEC028S             TO WRK-QTDEARQ.
           DISPLAY WRK-DISP9.

           MOVE 'RUECB028'                     TO WRK-DDNAME.
           MOVE 'O'                            TO WRK-I-O.
           MOVE 'REGISTROS SELECIONADOS'       TO WRK-DESCARQ.
           MOVE ACU-GRAVS-RUEC028S             TO WRK-QTDEARQ.
           DISPLAY WRK-DISP9.

           MOVE 'RUECB033'                     TO WRK-DDNAME.
           MOVE 'I'                            TO WRK-I-O.
           MOVE 'TOTAL LIDOS RUECB033'         TO WRK-DESCARQ.
           MOVE ACU-LIDOS-RUEC033S             TO WRK-QTDEARQ.
           DISPLAY WRK-DISP9.

           MOVE 'RUECB033'                     TO WRK-DDNAME.
           MOVE 'O'                            TO WRK-I-O.
           MOVE 'REGISTROS SELECIONADOS'       TO WRK-DESCARQ.
           MOVE ACU-GRAVS-RUEC033S             TO WRK-QTDEARQ.
           DISPLAY WRK-DISP9.

           MOVE 'RUECB034'                     TO WRK-DDNAME.
           MOVE 'I'                            TO WRK-I-O.
           MOVE 'TOTAL LIDOS RUECB034'         TO WRK-DESCARQ.
           MOVE ACU-LIDOS-RUEC034S             TO WRK-QTDEARQ.
           DISPLAY WRK-DISP9.

           MOVE 'RUECB034'                     TO WRK-DDNAME.
           MOVE 'O'                            TO WRK-I-O.
           MOVE 'REGISTROS SELECIONADOS'       TO WRK-DESCARQ.
           MOVE ACU-GRAVS-RUEC034S             TO WRK-QTDEARQ.
           DISPLAY WRK-DISP9.

           MOVE 'RUECB035'                     TO WRK-DDNAME.
           MOVE 'I'                            TO WRK-I-O.
           MOVE 'TOTAL LIDOS RUECB035'         TO WRK-DESCARQ.
           MOVE ACU-LIDOS-RUEC035S             TO WRK-QTDEARQ.
           DISPLAY WRK-DISP9.

           MOVE 'RUECB035'                     TO WRK-DDNAME.
           MOVE 'O'                            TO WRK-I-O.
           MOVE 'REGISTROS SELECIONADOS'       TO WRK-DESCARQ.
           MOVE ACU-GRAVS-RUEC035S             TO WRK-QTDEARQ.
           DISPLAY WRK-DISP9.

           MOVE 'RUECB036'                     TO WRK-DDNAME.
           MOVE 'I'                            TO WRK-I-O.
           MOVE 'TOTAL LIDOS RUECB036'         TO WRK-DESCARQ.
           MOVE ACU-LIDOS-RUEC036S             TO WRK-QTDEARQ.
           DISPLAY WRK-DISP9.

           MOVE 'RUECB036'                     TO WRK-DDNAME.
           MOVE 'O'                            TO WRK-I-O.
           MOVE 'REGISTROS SELECIONADOS'       TO WRK-DESCARQ.
           MOVE ACU-GRAVS-RUEC036S             TO WRK-QTDEARQ.
           DISPLAY WRK-DISP9.

           MOVE 'RUECB038'                     TO WRK-DDNAME.
           MOVE 'I'                            TO WRK-I-O.
           MOVE 'TOTAL LIDOS RUECB038'         TO WRK-DESCARQ.
           MOVE ACU-LIDOS-RUEC038S             TO WRK-QTDEARQ.
           DISPLAY WRK-DISP9.

           MOVE 'RUECB038'                     TO WRK-DDNAME.
           MOVE 'O'                            TO WRK-I-O.
           MOVE 'REGISTROS SELECIONADOS'       TO WRK-DESCARQ.
           MOVE ACU-GRAVS-RUEC038S             TO WRK-QTDEARQ.
           DISPLAY WRK-DISP9.

           MOVE 'RUECB040'                     TO WRK-DDNAME.
           MOVE 'I'                            TO WRK-I-O.
           MOVE 'TOTAL LIDOS RUECB040'         TO WRK-DESCARQ.
           MOVE ACU-LIDOS-RUEC040S             TO WRK-QTDEARQ.
           DISPLAY WRK-DISP9.

           MOVE 'RUECB040'                     TO WRK-DDNAME.
           MOVE 'O'                            TO WRK-I-O.
           MOVE 'REGISTROS SELECIONADOS'       TO WRK-DESCARQ.
           MOVE ACU-GRAVS-RUEC040S             TO WRK-QTDEARQ.
           DISPLAY WRK-DISP9.

           MOVE 'RUECB041'                     TO WRK-DDNAME.
           MOVE 'I'                            TO WRK-I-O.
           MOVE 'TOTAL LIDOS RUECB041'         TO WRK-DESCARQ.
           MOVE ACU-LIDOS-RUEC041S             TO WRK-QTDEARQ.
           DISPLAY WRK-DISP9.

           MOVE 'RUECB041'                     TO WRK-DDNAME.
           MOVE 'O'                            TO WRK-I-O.
           MOVE 'REGISTROS SELECIONADOS'       TO WRK-DESCARQ.
           MOVE ACU-GRAVS-RUEC041S             TO WRK-QTDEARQ.
           DISPLAY WRK-DISP9.

           MOVE 'RUECB044'                     TO WRK-DDNAME.
           MOVE 'I'                            TO WRK-I-O.
           MOVE 'TOTAL LIDOS RUECB044'         TO WRK-DESCARQ.
           MOVE ACU-LIDOS-RUEC044S             TO WRK-QTDEARQ.
           DISPLAY WRK-DISP9.

           MOVE 'RUECB044'                     TO WRK-DDNAME.
           MOVE 'O'                            TO WRK-I-O.
           MOVE 'REGISTROS SELECIONADOS'       TO WRK-DESCARQ.
           MOVE ACU-GRAVS-RUEC044S             TO WRK-QTDEARQ.
           DISPLAY WRK-DISP9.

           MOVE 'RUECB052'                     TO WRK-DDNAME.
           MOVE 'I'                            TO WRK-I-O.
           MOVE 'TOTAL LIDOS RUECB052'         TO WRK-DESCARQ.
           MOVE ACU-LIDOS-RUEC052S             TO WRK-QTDEARQ.
           DISPLAY WRK-DISP9.

           MOVE 'RUECB052'                     TO WRK-DDNAME.
           MOVE 'O'                            TO WRK-I-O.
           MOVE 'REGISTROS SELECIONADOS'       TO WRK-DESCARQ.
           MOVE ACU-GRAVS-RUEC052S             TO WRK-QTDEARQ.
           DISPLAY WRK-DISP9.

           MOVE 'RUECB053'                     TO WRK-DDNAME.
           MOVE 'I'                            TO WRK-I-O.
           MOVE 'TOTAL LIDOS RUECB053'         TO WRK-DESCARQ.
           MOVE ACU-LIDOS-RUEC053S             TO WRK-QTDEARQ.
           DISPLAY WRK-DISP9.

           MOVE 'RUECB053'                     TO WRK-DDNAME.
           MOVE 'O'                            TO WRK-I-O.
           MOVE 'REGISTROS SELECIONADOS'       TO WRK-DESCARQ.
           MOVE ACU-GRAVS-RUEC053S             TO WRK-QTDEARQ.
           DISPLAY WRK-DISP9.

           MOVE 'RUECB054'                     TO WRK-DDNAME.
           MOVE 'I'                            TO WRK-I-O.
           MOVE 'TOTAL LIDOS RUECB054'         TO WRK-DESCARQ.
           MOVE ACU-LIDOS-RUEC054S             TO WRK-QTDEARQ.
           DISPLAY WRK-DISP9.

           MOVE 'RUECB054'                     TO WRK-DDNAME.
           MOVE 'O'                            TO WRK-I-O.
           MOVE 'REGISTROS SELECIONADOS'       TO WRK-DESCARQ.
           MOVE ACU-GRAVS-RUEC054S             TO WRK-QTDEARQ.
           DISPLAY WRK-DISP9.

           MOVE 'RUECB064'                     TO WRK-DDNAME.
           MOVE 'I'                            TO WRK-I-O.
           MOVE 'TOTAL LIDOS RUECB064'         TO WRK-DESCARQ.
           MOVE ACU-LIDOS-RUEC064S             TO WRK-QTDEARQ.
           DISPLAY WRK-DISP9.

           MOVE 'RUECB064'                     TO WRK-DDNAME.
           MOVE 'O'                            TO WRK-I-O.
           MOVE 'REGISTROS SELECIONADOS'       TO WRK-DESCARQ.
           MOVE ACU-GRAVS-RUEC064S             TO WRK-QTDEARQ.
           DISPLAY WRK-DISP9.

           MOVE 'RUECB065'                     TO WRK-DDNAME.
           MOVE 'I'                            TO WRK-I-O.
           MOVE 'TOTAL LIDOS RUECB065'         TO WRK-DESCARQ.
           MOVE ACU-LIDOS-RUEC065S             TO WRK-QTDEARQ.
           DISPLAY WRK-DISP9.

           MOVE 'RUECB065'                     TO WRK-DDNAME.
           MOVE 'O'                            TO WRK-I-O.
           MOVE 'REGISTROS SELECIONADOS'       TO WRK-DESCARQ.
           MOVE ACU-GRAVS-RUEC065S             TO WRK-QTDEARQ.
           DISPLAY WRK-DISP9.

           MOVE 'RUECB066'                     TO WRK-DDNAME.
           MOVE 'I'                            TO WRK-I-O.
           MOVE 'TOTAL LIDOS RUECB066'         TO WRK-DESCARQ.
           MOVE ACU-LIDOS-RUEC066S             TO WRK-QTDEARQ.
           DISPLAY WRK-DISP9.

           MOVE 'RUECB066'                     TO WRK-DDNAME.
           MOVE 'O'                            TO WRK-I-O.
           MOVE 'REGISTROS SELECIONADOS'       TO WRK-DESCARQ.
           MOVE ACU-GRAVS-RUEC066S             TO WRK-QTDEARQ.
           DISPLAY WRK-DISP9.

           MOVE 'RUECB073'                     TO WRK-DDNAME.
           MOVE 'I'                            TO WRK-I-O.
           MOVE 'TOTAL LIDOS RUECB073'         TO WRK-DESCARQ.
           MOVE ACU-LIDOS-RUEC073S             TO WRK-QTDEARQ.
           DISPLAY WRK-DISP9.

           MOVE 'RUECB073'                     TO WRK-DDNAME.
           MOVE 'O'                            TO WRK-I-O.
           MOVE 'REGISTROS SELECIONADOS'       TO WRK-DESCARQ.
           MOVE ACU-GRAVS-RUEC073S             TO WRK-QTDEARQ.
           DISPLAY WRK-DISP9.

           MOVE 'RUECB074'                     TO WRK-DDNAME.
           MOVE 'I'                            TO WRK-I-O.
           MOVE 'TOTAL LIDOS RUECB074'         TO WRK-DESCARQ.
           MOVE ACU-LIDOS-RUEC074S             TO WRK-QTDEARQ.
           DISPLAY WRK-DISP9.

           MOVE 'RUECB074'                     TO WRK-DDNAME.
           MOVE 'O'                            TO WRK-I-O.
           MOVE 'REGISTROS SELECIONADOS'       TO WRK-DESCARQ.
           MOVE ACU-GRAVS-RUEC074S             TO WRK-QTDEARQ.
           DISPLAY WRK-DISP9.

           MOVE 'RUECB075'                     TO WRK-DDNAME.
           MOVE 'I'                            TO WRK-I-O.
           MOVE 'TOTAL LIDOS RUECB075'         TO WRK-DESCARQ.
           MOVE ACU-LIDOS-RUEC075S             TO WRK-QTDEARQ.
           DISPLAY WRK-DISP9.

           MOVE 'RUECB075'                     TO WRK-DDNAME.
           MOVE 'O'                            TO WRK-I-O.
           MOVE 'REGISTROS SELECIONADOS'       TO WRK-DESCARQ.
           MOVE ACU-GRAVS-RUEC075S             TO WRK-QTDEARQ.
           DISPLAY WRK-DISP9.

           MOVE 'RUECB096'                     TO WRK-DDNAME.
           MOVE 'I'                            TO WRK-I-O.
           MOVE 'TOTAL LIDOS RUECB096'         TO WRK-DESCARQ.
           MOVE ACU-LIDOS-RUEC096S             TO WRK-QTDEARQ.
           DISPLAY WRK-DISP9.

           MOVE 'RUECB096'                     TO WRK-DDNAME.
           MOVE 'O'                            TO WRK-I-O.
           MOVE 'REGISTROS SELECIONADOS'       TO WRK-DESCARQ.
           MOVE ACU-GRAVS-RUEC096S             TO WRK-QTDEARQ.
           DISPLAY WRK-DISP9.

           MOVE 'RUECB097'                     TO WRK-DDNAME.
           MOVE 'I'                            TO WRK-I-O.
           MOVE 'TOTAL LIDOS RUECB097'         TO WRK-DESCARQ.
           MOVE ACU-LIDOS-RUEC097S             TO WRK-QTDEARQ.
           DISPLAY WRK-DISP9.

           MOVE 'RUECB097'                     TO WRK-DDNAME.
           MOVE 'O'                            TO WRK-I-O.
           MOVE 'REGISTROS SELECIONADOS'       TO WRK-DESCARQ.
           MOVE ACU-GRAVS-RUEC097S             TO WRK-QTDEARQ.
           DISPLAY WRK-DISP9.

           MOVE 'RUECB099'                     TO WRK-DDNAME.
           MOVE 'I'                            TO WRK-I-O.
           MOVE 'TOTAL LIDOS RUECB099'         TO WRK-DESCARQ.
           MOVE ACU-LIDOS-RUEC099S             TO WRK-QTDEARQ.
           DISPLAY WRK-DISP9.

           MOVE 'RUECB099'                     TO WRK-DDNAME.
           MOVE 'O'                            TO WRK-I-O.
           MOVE 'REGISTROS SELECIONADOS'       TO WRK-DESCARQ.
           MOVE ACU-GRAVS-RUEC099S             TO WRK-QTDEARQ.
           DISPLAY WRK-DISP9.

           MOVE 'RUECB09A'                     TO WRK-DDNAME.
           MOVE 'I'                            TO WRK-I-O.
           MOVE 'TOTAL LIDOS RUECB09A'         TO WRK-DESCARQ.
           MOVE ACU-LIDOS-RUEC09AS             TO WRK-QTDEARQ.
           DISPLAY WRK-DISP9.

           MOVE 'RUECB09A'                     TO WRK-DDNAME.
           MOVE 'O'                            TO WRK-I-O.
           MOVE 'REGISTROS SELECIONADOS'       TO WRK-DESCARQ.
           MOVE ACU-GRAVS-RUEC09AS             TO WRK-QTDEARQ.
           DISPLAY WRK-DISP9.

           MOVE 'RUECB09B'                     TO WRK-DDNAME.
           MOVE 'I'                            TO WRK-I-O.
           MOVE 'TOTAL LIDOS RUECB09B'         TO WRK-DESCARQ.
           MOVE ACU-LIDOS-RUEC09BS             TO WRK-QTDEARQ.
           DISPLAY WRK-DISP9.

           MOVE 'RUECB09B'                     TO WRK-DDNAME.
           MOVE 'O'                            TO WRK-I-O.
           MOVE 'REGISTROS SELECIONADOS'       TO WRK-DESCARQ.
           MOVE ACU-GRAVS-RUEC09BS             TO WRK-QTDEARQ.
           DISPLAY WRK-DISP9.

           MOVE 'RUECB09D'                     TO WRK-DDNAME.
           MOVE 'I'                            TO WRK-I-O.
           MOVE 'TOTAL LIDOS RUECB09D'         TO WRK-DESCARQ.
           MOVE ACU-LIDOS-RUEC09DS             TO WRK-QTDEARQ.
           DISPLAY WRK-DISP9.

           MOVE 'RUECB09D'                     TO WRK-DDNAME.
           MOVE 'O'                            TO WRK-I-O.
           MOVE 'REGISTROS SELECIONADOS'       TO WRK-DESCARQ.
           MOVE ACU-GRAVS-RUEC09DS             TO WRK-QTDEARQ.
           DISPLAY WRK-DISP9.

           MOVE 'RUECB09E'                     TO WRK-DDNAME.
           MOVE 'I'                            TO WRK-I-O.
           MOVE 'TOTAL LIDOS RUECB09E'         TO WRK-DESCARQ.
           MOVE ACU-LIDOS-RUEC09ES             TO WRK-QTDEARQ.
           DISPLAY WRK-DISP9.

           MOVE 'RUECB09E'                     TO WRK-DDNAME.
           MOVE 'O'                            TO WRK-I-O.
           MOVE 'REGISTROS SELECIONADOS'       TO WRK-DESCARQ.
           MOVE ACU-GRAVS-RUEC09ES             TO WRK-QTDEARQ.
           DISPLAY WRK-DISP9.

           MOVE 'RUECB09F'                     TO WRK-DDNAME.
           MOVE 'I'                            TO WRK-I-O.
           MOVE 'TOTAL LIDOS RUECB09F'         TO WRK-DESCARQ.
           MOVE ACU-LIDOS-RUEC09FS             TO WRK-QTDEARQ.
           DISPLAY WRK-DISP9.

           MOVE 'RUECB09F'                     TO WRK-DDNAME.
           MOVE 'O'                            TO WRK-I-O.
           MOVE 'REGISTROS SELECIONADOS'       TO WRK-DESCARQ.
           MOVE ACU-GRAVS-RUEC09FS             TO WRK-QTDEARQ.
           DISPLAY WRK-DISP9.

           MOVE 'RUECB09G'                     TO WRK-DDNAME.
           MOVE 'I'                            TO WRK-I-O.
           MOVE 'TOTAL LIDOS RUECB09G'         TO WRK-DESCARQ.
           MOVE ACU-LIDOS-RUEC09GS             TO WRK-QTDEARQ.
           DISPLAY WRK-DISP9.

           MOVE 'RUECB09G'                     TO WRK-DDNAME.
           MOVE 'O'                            TO WRK-I-O.
           MOVE 'REGISTROS SELECIONADOS'       TO WRK-DESCARQ.
           MOVE ACU-GRAVS-RUEC09GS             TO WRK-QTDEARQ.
           DISPLAY WRK-DISP9.

           MOVE 'RUECB09H'                     TO WRK-DDNAME.
           MOVE 'I'                            TO WRK-I-O.
           MOVE 'TOTAL LIDOS RUECB09H'         TO WRK-DESCARQ.
           MOVE ACU-LIDOS-RUEC09HS             TO WRK-QTDEARQ.
           DISPLAY WRK-DISP9.

           MOVE 'RUECB09H'                     TO WRK-DDNAME.
           MOVE 'O'                            TO WRK-I-O.
           MOVE 'REGISTROS SELECIONADOS'       TO WRK-DESCARQ.
           MOVE ACU-GRAVS-RUEC09HS             TO WRK-QTDEARQ.
           DISPLAY WRK-DISP9.

           MOVE 'RUECB09I'                     TO WRK-DDNAME.
           MOVE 'I'                            TO WRK-I-O.
           MOVE 'TOTAL LIDOS RUECB09I'         TO WRK-DESCARQ.
           MOVE ACU-LIDOS-RUEC09IS             TO WRK-QTDEARQ.
           DISPLAY WRK-DISP9.

           MOVE 'RUECB09I'                     TO WRK-DDNAME.
           MOVE 'O'                            TO WRK-I-O.
           MOVE 'REGISTROS SELECIONADOS'       TO WRK-DESCARQ.
           MOVE ACU-GRAVS-RUEC09IS             TO WRK-QTDEARQ.
           DISPLAY WRK-DISP9.

           DISPLAY WRK-DISP3.

           MOVE WHEN-COMPILED                  TO WRK-COMPILACAO.
           MOVE WRK-DTCOMPILA                  TO WRK-DATA-COMP.
           MOVE WRK-DTCOMPILA(4:2)             TO WRK-DATA-COMP(1:2).
           MOVE WRK-DTCOMPILA(1:2)             TO WRK-DATA-COMP(4:2).
           MOVE WRK-HRCOMPILA                  TO WRK-HORA-COMP.
           DISPLAY WRK-DISP16.
           DISPLAY WRK-DISP1.

      *---------------------------------------------------------------*
       9100-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       9999-ROTINA-ERRO                SECTION.
      *---------------------------------------------------------------*

           MOVE    'RUEC1450'           TO ERR-PGM

           IF  ERR-TIPO-ACESSO         EQUAL 'DB2'
               CALL   'BRAD7100'       USING WRK-BATCH
                                             ERRO-AREA
                                             SQLCA
           ELSE
               CALL   'BRAD7100'       USING WRK-BATCH
                                             ERRO-AREA
           END-IF.

           GOBACK.

      *---------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *---------------------------------------------------------------*
