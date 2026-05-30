      ******************************************************************
      * NOME BOOK : CMRUWA0A                                           *
      * DESCRICAO : AREA INTERFACE SERVICO BASICO                      *
      *             CONSULTA TODAS AS AGENCIAS MIGRADAS E NAO MIGRADAS *
      *             TAB.: AGENCIAS CREDITO CONVIVENCIA                 *
      *                   (TAG_CREDT_CVIVE)                            *
      * DATA      : FEVEREIRO/2013                                     *
      * AUTOR     : ODIRLEI COSTA                                      *
      * EMPRESA   : BSI TECNOLOGIA                                     *
      * GRUPO     : CMRU                                               *
      * TAMANHO   : 14967                                              *
      ******************************************************************
      * CMRUWA0A-REGISTRO-ENTRADA                                      *
      * CMRUWA0A-E-OPC-PESQ             = OPCAO DE PESQUISA            *
      *                                   1 - POR AGENCIA LEGADO       *
      *                                   2 - POR AGENCIA NOVA ARQ.    *
      *                                   3 - POR DATA DE MIGRACAO     *
      *                                   4 - POR SITUACAO DA AGENCIA  *
      *                                   5 - TODAS AS AGENCIAS        *
      * CMRUWA0A-E-CPSSOA-JURID         = CODIGO PESSOA JURIDICA       *
      * CMRUWA0A-E-NSEQ-UND-ORGNZ       = NUMERO SEQUENCIAL UORG       *
      * CMRUWA0A-E-DMGRAC-AG-RURAL      = DATA DE MIGRACAO             *
      * CMRUWA0A-E-CSIT-REG             = CODIGO DE SITUACAO           *
      *                                   A - ATIVA                    *
      *                                   I - INATIVA                  *
      *                                   B - BLOQUEADA                *
      * CMRUWA0A-E-CUSUAR-INCL          = CODIGO DO USUARIO DE INCLUSAO*
      * CMRUWA0A-E-HINCL-REG            = MOMENTO DA INCLUSAO          *
      * CMRUWA0A-E-CUSUAR-MANUT         = CODIGO USUARIO DE MANUTENCAO *
      * CMRUWA0A-E-HMANUT-REG           = MOMENTO DA MANUTENCAO        *
      *                                                                *
      * CMRUWA0A-REGISTRO-SAIDA                                        *
      * CMRUWA0A-S-COD-RETORNO          = CODIGO DE RETORNO DO MODULO  *
      * CMRUWA0A-S-COD-ERRO             = CODIGO DO ERRO               *
      * CMRUWA0A-S-COD-MENSAGEM         = CODIGO DA MENSAGEM DE ERRO   *
      * CMRUWA0A-S-QTD-LISTA-OCORR      = QUANTIDADE DE OCORRENCIAS    *
      * CMRUWA0A-S-CPSSOA-JURID         = CODIGO PESSOA JURIDICA       *
      * CMRUWA0A-S-NSEQ-UND-ORGNZ       = NUMERO SEQUENCIAL UORG       *
      * CMRUWA0A-S-DMGRAC-AG-RURAL      = DATA DE MIGRACAO             *
      * CMRUWA0A-S-CSIT-REG             = CODIGO DE SITUACAO           *
      *                                   A - ATIVA                    *
      *                                   I - INATIVA                  *
      *                                   B - BLOQUEADA                *
      * CMRUWA0A-S-CUSUAR-INCL          = CODIGO DO USUARIO DE INCLUSAO*
      * CMRUWA0A-S-HINCL-REG            = MOMENTO DA INCLUSAO          *
      * CMRUWA0A-S-CUSUAR-MANUT         = CODIGO USUARIO DE MANUTENCAO *
      * CMRUWA0A-S-HMANUT-REG           = MOMENTO DA MANUTENCAO        *
      ******************************************************************
        05 CMRUWA0A-HEADER.
           10 CMRUWA0A-E-COD-LAYOUT                PIC X(08) VALUE
                                                   'CMRUWA0A'.
           10 CMRUWA0A-E-TAM-LAYOUT                PIC 9(05) VALUE
                                                   14967.
        05 CMRUWA0A-REGISTRO-ENTRADA.
          10 CMRUWA0A-E-OPC-PESQ                   PIC 9(01).
          10 CMRUWA0A-E-CPSSOA-JURID               PIC 9(10).
          10 CMRUWA0A-E-NSEQ-UND-ORGNZ             PIC 9(08).
          10 CMRUWA0A-E-DMGRAC-AG-RURAL            PIC X(10).
          10 CMRUWA0A-E-CSIT-REG                   PIC X(01).
          10 CMRUWA0A-E-CUSUAR-INCL                PIC X(09).
          10 CMRUWA0A-E-HINCL-REG                  PIC X(26).
          10 CMRUWA0A-E-CUSUAR-MANUT               PIC X(09).
          10 CMRUWA0A-E-HMANUT-REG                 PIC X(26).

        05 CMRUWA0A-BLOCO-PAGINACAO.
          10 CMRUWA0A-INDICADOR-PAGINACAO          PIC X(01).
             88 CMRUWA0A-P-INICIAL                 VALUE 'I'.
             88 CMRUWA0A-P-PRIMEIRA                VALUE 'P'.
             88 CMRUWA0A-P-SEGUINTE                VALUE 'S'.
             88 CMRUWA0A-P-ANTERIOR                VALUE 'A'.
             88 CMRUWA0A-P-ULTIMA                  VALUE 'U'.

        05 CMRUWA0A-REGISTRO-SAIDA.
           10 CMRUWA0A-S-COD-RETORNO               PIC 9(02).
           10 CMRUWA0A-S-COD-ERRO                  PIC X(04).
           10 CMRUWA0A-S-COD-MENSAGEM              PIC X(08).
           10 CMRUWA0A-S-QTD-LISTA-OCORR           PIC 9(03).
           10 CMRUWA0A-S-REG-LISTA-OCORR           OCCURS 150 TIMES
                            DEPENDING ON    CMRUWA0A-S-QTD-LISTA-OCORR.
             15 CMRUWA0A-S-CPSSOA-JURID            PIC 9(10).
             15 CMRUWA0A-S-NSEQ-UND-ORGNZ          PIC 9(08).
             15 CMRUWA0A-S-DMGRAC-AG-RURAL         PIC X(10).
             15 CMRUWA0A-S-CSIT-REG                PIC X(01).
             15 CMRUWA0A-S-CUSUAR-INCL             PIC X(09).
             15 CMRUWA0A-S-HINCL-REG               PIC X(26).
             15 CMRUWA0A-S-CUSUAR-MANUT            PIC X(09).
             15 CMRUWA0A-S-HMANUT-REG              PIC X(26).
      ******************************************************************
      *                        FINAL DO BOOK                           *
      ******************************************************************
