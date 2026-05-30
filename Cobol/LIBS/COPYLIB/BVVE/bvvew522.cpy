      *================================================================*
      *  RESOLUCAO 4734 - SOLICITACAO DE CESSAO DE RECEBIVEIS          *
      *----------------------------------------------------------------*
      *  COMPONENTE...: BVVEW522 - AREA DE ACESSO AO BVVEA522          *
      *  DESCRICAO....: SOLICITACAO DE CESSAO DE RECEBIVEIS            *
      *                (BASEADO NO BOOK I#BVVEND - BVVEA504)           *
      *                                                                *
      *--------------------- AREA DE ENTRADA --------------------------*
      *                                                                *
      *  PROTOCOLO.....: ENVIAR EM BRANCO NA PRIMEIRA VEZ E O BVVEA522 *
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
      *--------------------- AREA DE SAIDA ----------------------------*
      *                                                                *
      *  COD-RETORNO...: 00 - PROCESSAMENTO OK                         *
      *                  08 - ERRO CONSISTENCIA                        *
      *                  10 - GRADE FECHADA                            *
      *                  11 - ERRO DE COMUNICACAO COM A CIP            *
      *                  12 - ERRO DE DB2                              *
      *                  16 - ERRO DE MODULO                           *
      *                                                                *
      *  MSG-RETORNO...: MENSAGEM DE RETORNO                           *
      *  PROTOCLO......: GERADO NO PRIMEIRO ACESSO P/ CTRL COMUNICACAO *
      *  TMP ESPERA....: TEMPO LIMITE DE ESPERA DO CANAL (HH:MM:SS)    *
      *                                                                *
      *================================================================*
      *                                                                *
       05 BVVEW522-HEADER.
          10 BVVEW522-COD-LAYOUT           PIC X(008) VALUE 'BVVEW522'.
          10 BVVEW522-TAM-LAYOUT           PIC 9(005) VALUE 01546.
      *
       05 BVVEW522-REGISTRO.
          10 BVVEW522-BLOCO-ENTRADA.
             15 BVVEW522-E-PROTOCOLO       PIC  X(050).
             15 BVVEW522-E-AGENCIA         PIC  9(005).
             15 BVVEW522-E-CONTA           PIC  9(013).
             15 BVVEW522-E-CANAL           PIC  X(004).
             15 BVVEW522-E-CONTRATO        PIC  X(019).
             15 BVVEW522-E-IND-I-A         PIC  X(001).
             15 BVVEW522-E-DT-CONT         PIC  X(010).
             15 BVVEW522-E-VLR-TOT         PIC  9(015)V9(002).
             15 BVVEW522-E-IND-ORIG-MASSV  PIC  X(001).
             15 BVVEW522-E-NSMULA-OPER     PIC  9(009).
             15 BVVEW522-E-OPERACAO.
                20 BVVEW522-E-OPER-ANO     PIC  9(004).
                20 BVVEW522-E-OPER-SEQ     PIC  9(009).
             15 BVVEW522-E-FIM-OCOR        PIC  X(001).
             15 BVVEW522-E-OCORRENCIAS     OCCURS 040 TIMES.
                20 BVVEW522-E-PRODUTO      PIC  9(005).
                20 BVVEW522-E-DATA         PIC  X(010).
                20 BVVEW522-E-VLR-DESCONTO PIC  9(015)V9(002).

          10 BVVEW522-BLOCO-SAIDA.
             15 BVVEW522-S-COD-RETORNO     PIC  9(002).
             15 BVVEW522-S-MSG-RETORNO     PIC  X(050).
             15 BVVEW522-S-PROTOCOLO       PIC  X(050).
             15 BVVEW522-S-TMP-ESPERA      PIC  X(008).
