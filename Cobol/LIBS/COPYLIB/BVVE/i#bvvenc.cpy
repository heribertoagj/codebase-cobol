      ******************************************************************
      *  NOVOS NORMATIVOS - RESOLUCAO 4734                             *
      *                                                                *
      ******************************************************************
      *                                                                *
      *  NOME DO BOOK..: I#BVVENC - AREA DO MODULO BVVEA503            *
      *  DESCRICAO.....: OBTER A AGENDA DE RECEBIVEIS DO CLIENTE       *
      *  TAMANHO.......: 03286 BYTES                                   *
      *                                                                *
      *--ENTRADA-------------------------------------------------------*
      *                                                                *
      *  AGENCIA.......: AGENCIA DA AGENDA DE RECEBIVEIS               *
      *  CONTA.........: CONTA DA AGENDA DE RECEBIVEIS                 *
      *  PRODUTO ......: PRODUTO P/ FILTRO DA AGENDA                   *
      *  DATA-INI......: DATA INICIO P/ FILTRO DA AGENDA (DD.MM.AAAA)  *
      *  DATA-FIM......: DATA FIM P/ FILTRO DA AGENDA    (DD.MM.AAAA)  *
      *  VLR-OPERACAO..: VALOR TOTAL P/ FILTRO DA AGENDA               *
      *  VLR-OPERACAO..: VALOR TOTAL P/ FILTRO DA AGENDA               *
      *  VLR-MIN-UR....: VALOR MINIMO P/ FILTRO DA UR                  *
      *  VLR-MAX-UR....: VALOR MAXIMO P/ FILTRO DA UR                  *
      *                                                                *
      *--RESTART-------------------------------------------------------*
      *                                                                *
      *  ENVIAR ZEROS E ESPACOS NO PRIMEIRO ACESSO                     *
      *  PROD-REST.....: PRODUTO PARA RESTART                          *
      *  DT-REST.......: DATA PARA RESTART                             *
      *                                                                *
      *--SAIDA---------------------------------------------------------*
      *                                                                *
      *  COD-RETORNO...: 00 - PROCESSAMENTO OK                         *
      *                  01 - RESTART                                  *
      *                  08 - ERRO CONSISTENCIA                        *
      *                  12 - AGENDA NAO ENCONTRADA                    *
      *                  16 - ERRO DE DB2                              *
      *                                                                *
      *  MSG-RETORNO...: MENSAGEM DE RETORNO                           *
      *  QTDE-OCORR....: QUANTIDADE DE OCORRENCIAS RETORNADAS          *
      *  PRODUTO.......: PRODUTO DO RECEBIVEL                          *
      *  DESC-PROD.....: DESCRICAO PRODUTO                             *
      *  DATA..........: DATA DO RECEBIVEL (DD.MM.AAAA)                *
      *  VLR DISP......: VALOR DISPONIVEL PARA OPERACOES               *
      *                                                                *
      ******************************************************************
      *                                                                *
       01  BVVENC-COMMAREA.
           05 BVVENC-AREA-ENTRADA.
              10 BVVENC-E-AGENCIA                      PIC  9(005).
              10 BVVENC-E-CONTA                        PIC  9(013).
              10 BVVENC-E-PRODUTO                      PIC  9(005).
              10 BVVENC-E-DT-INI                       PIC  X(010).
              10 BVVENC-E-DT-FIM                       PIC  X(010).
              10 BVVENC-E-VLR-OPERACAO                 PIC  9(015)V9(2).
              10 BVVENC-E-VLR-MIN-UR                   PIC  9(015)V9(2).
              10 BVVENC-E-VLR-MAX-UR                   PIC  9(015)V9(2).

           05 BVVENC-AREA-RESTART.
              10 BVVENC-R-PROD-REST                    PIC  9(005).
              10 BVVENC-R-DT-REST                      PIC  X(010).

           05 BVVENC-AREA-SAIDA.
              10 BVVENC-S-COD-RETORNO                  PIC  9(002).
              10 BVVENC-S-MSG-RETORNO                  PIC  X(050).
              10 BVVENC-S-QTDE-OCORR                   PIC  9(005).
              10 BVVENC-S-OCORRENCIAS   OCCURS 60 TIMES.
                 15 BVVENC-S-PRODUTO                   PIC  9(005).
                 15 BVVENC-S-DESC-PROD                 PIC  X(020).
                 15 BVVENC-S-DATA                      PIC  X(010).
                 15 BVVENC-S-VLR-DISP                  PIC  9(015)V9(2).
