      ******************************************************************
      *  NOVOS NORMATIVOS - RESOLUCAO 4734                             *
      *                                                                *
      ******************************************************************
      *                                                                *
      *  NOME DO BOOK..: I#BVVEND - AREA DO MODULO BVVEA504            *
      *  DESCRICAO.....: SOLICITACAO DE CESSAO DE RECEBIVEIS           *
      *  TAMANHO.......: 01533 BYTES                                   *
      *                                                                *
      *--ENTRADA-------------------------------------------------------*
      *                                                                *
      *  PROTOCOLO.....: ENVIAR EM BRANCO NA PRIMEIRA VEZ E O BVVEA504 *
      *                  INFORMARA O PROTOCOLO NO RETORNO. NAS PROXIMAS*
      *                  CHAMADAS DEVERA SER INFORMADO                 *
      *  AGENCIA.......: AGENCIA DETENTORA DA AGENDA DE RECEBIVEIS     *
      *  CONTA.........: CONTA DETENTORA DA AGENDA DE RECEBIVEIS       *
      *  CANAL.........: CANAL SOLICITANTE                             *
      *  CONTRATO......: NUMERO DO CONTRATO/OPERACAO                   *
      *  IND-I-A.......: INDICADOR DE INCLUSAO 'I' OU ALTERACAO 'A'    *
      *  DT CONT.......: DATA DE VENCIMENTO DO CONTRATO (DD.MM.AAAA)   *
      *  VLR TOT.......: VALOR TOTAL DO CONTRATO                       *
      *  IND-ORIG-MASSV: INDICADOR DA ORIGEM DA OPERACAO MASSIVA       *
      *                  'C' CANAL (MOBILE / NET EMPRESA)              *
      *                  'G' TERMINAL GERENCIAL                        *
      *  NSMULA-OPER...: NUMERO DE SIMULACAO DA OPERACAO               *
      *                  DEVE SER PREENCHIDO QUANDO CHAMADO PELO CANAL *
      *  OPERACAO......: NUMERO DA OPERACAO                            *
      *                  DEVE SER PREENCHIDO QUANDO CHAMADO PELO TG    *
      *  FIM-OCOR......: INDICA QUANDO ACABARAM AS OCORRENCIAS         *
      *                                                                *
      *  OCORRENCIAS (CONFORME MODULO BVVEA503)                        *
      *   PRODUTO......: PRODUTO PARA MARCACAO                         *
      *   DATA.........: DATA DO RECEBIVEL PARA MARCACAO (DD.MM.AAAA)  *
      *   VALOR CESSAO.: VALOR PARA MARCACAO (CESSAO)                  *
      *                                                                *
      *--SAIDA---------------------------------------------------------*
      *                                                                *
      *  COD-RETORNO...: 00 - PROCESSAMENTO OK                         *
      *                  08 - ERRO CONSISTENCIA                        *
      *                  10 - GRADE FECHADA                            *
      *                  11 - ERRO DE COMUNICACAO COM A CIP            *
      *                  16 - ERRO DE DB2                              *
      *                                                                *
      *  MSG-RETORNO...: MENSAGEM DE RETORNO                           *
      *  PROTOCLO......: GERADO NO PRIMEIRO ACESSO P/ CTRL COMUNICACAO *
      *  TMP ESPERA....: TEMPO LIMITE DE ESPERA DO CANAL (HH:MM:SS)    *
      *                                                                *
      ******************************************************************
      *                                                                *
       01  BVVEND-COMMAREA.
           05 BVVEND-AREA-ENTRADA.
              10 BVVEND-E-PROTOCOLO                    PIC  X(050).
              10 BVVEND-E-AGENCIA                      PIC  9(005).
              10 BVVEND-E-CONTA                        PIC  9(013).
              10 BVVEND-E-CANAL                        PIC  X(004).
              10 BVVEND-E-CONTRATO                     PIC  X(019).
              10 BVVEND-E-IND-I-A                      PIC  X(001).
              10 BVVEND-E-DT-CONT                      PIC  X(010).
              10 BVVEND-E-VLR-TOT                      PIC  9(015)V99.
              10 BVVEND-E-IND-ORIG-MASSV               PIC  X(001).
              10 BVVEND-E-NSMULA-OPER                  PIC  9(009).
              10 BVVEND-E-OPERACAO.
                 15 BVVEND-E-OPER-ANO                  PIC  9(004).
                 15 BVVEND-E-OPER-SEQ                  PIC  9(009).
              10 BVVEND-E-FIM-OCOR                     PIC  X(001).
              10 BVVEND-E-OCORRENCIAS   OCCURS 40 TIMES.
                 15 BVVEND-E-PRODUTO                   PIC  9(005).
                 15 BVVEND-E-DATA                      PIC  X(010).
                 15 BVVEND-E-VLR-DESCONTO              PIC  9(015)V9(2).

           05 BVVEND-AREA-SAIDA.
              10 BVVEND-S-COD-RETORNO                  PIC  9(002).
              10 BVVEND-S-MSG-RETORNO                  PIC  X(050).
              10 BVVEND-S-PROTOCOLO                    PIC  X(050).
              10 BVVEND-S-TMP-ESPERA                   PIC  X(008).
