      ******************************************************************
      *  NOVOS NORMATIVOS - RESOLUCAO 4734                             *
      *                                                                *
      ******************************************************************
      *                                                                *
      *  NOME DO BOOK.: I#BVVENN - AREA DO MODULO BVVEA514             *
      *  DESCRICAO....: SOLICITACAO DE GRAVAME                         *
      *  TAMANHO......: 01216 BYTES                                    *
      *                                                                *
      *--ENTRADA-------------------------------------------------------*
      *                                                                *
      *  PROTOCOLO....: ENVIAR EM BRANCO NA PRIMEIRA CHAMADA.          *
      *                 NAS CHAMADAS SEGUINTES ENVIAR O PROTOCOLO      *
      *                 RECEBIDO NA AREA DE SAIDA (NA PRIMEIRA VEZ)    *
      *  CONTRATO.....: NUMERO DO CONTRATO GRAN                        *
      *  DT CONT......: DATA DE VENCIMENTO DO CONTRATO (DD.MM.AAAA)    *
      *  VLR TOT......: VALOR TOTAL DO CONTRATO                        *
      *  VLR GAR......: VALOR GARANTIA                                 *
      *  DT INICIO....: DT INICIO P/ BLOQUEIO DE GARANTIAS (DD.MM.AAAA)*
      *  DT FIM.......: DT FINAL P/BLOQUEIO DE GARANTIAS (DD.MM.AAAA)  *
      *  IND-I-A......: INDICA SE EH CONTRATO NOVO (I) OU ALTERACAO (A)*
      *  FIM-OCORR....: INDICA FINAL DAS OCORRENCIAS / ULTIMA CHAMADA  *
      *  IND-RENEG....: INDICA SE TEM RENEGOCIACAO                     *
      *                                                                *
      *  OCORRENCIAS                                                   *
      *   AGENCIA......: AGENCIA DETENTORA DA AGENDA DE RECEBIVEIS     *
      *   CONTA........: CONTA DETENTORA DA AGENDA DE RECEBIVEIS       *
      *   BANDEIRA....: BANDEIRA CORPORATIVA VISAO 2                   *
      *   VLR BAND....: VALOR A SER GRAVAMADO PARA BANDEIRA            *
      *                                                                *
      *--SAIDA---------------------------------------------------------*
      *                                                                *
      *  COD-RETORNO...: CODIGO DE RETORNO                             *
      *  MSG-RETORNO...: MENSGEM DE RETORNO                            *
      *  PROTOCOLO.....: PROTOCOLO PARA RASTREIO DE SOLICITACAO        *
      *  TMP ESPERA....: TEMPO LIMITE DE ESPERA DO CANAL (HH:MM:SS)    *
      *                                                                *
      ******************************************************************
      *                                                                *
       01  BVVENN-COMMAREA.
           05 BVVENN-AREA-ENTRADA.
              10 BVVENN-E-PROTOCOLO                    PIC  X(050).
              10 BVVENN-E-CCUSTO                       PIC  X(004).
              10 BVVENN-E-CONTRATO                     PIC  X(019).
              10 BVVENN-E-DT-FIM-CONTR                 PIC  X(010).
              10 BVVENN-E-VLR-TOT                      PIC  9(015)V99.
              10 BVVENN-E-VLR-GAR                      PIC  9(015)V99.
              10 BVVENN-E-DT-INI-GRAV                  PIC  X(010).
              10 BVVENN-E-DT-FIM-GRAV                  PIC  X(010).
              10 BVVENN-E-IND-I-A                      PIC  X(001).
              10 BVVENN-E-IND-RENEG                    PIC  X(001).
              10 BVVENN-E-AUT-CESSAO-GRAV              PIC  X(001).
              10 BVVENN-E-AUT-CNPJ-CPF                 PIC  9(009).
              10 BVVENN-E-AUT-FILIAL                   PIC  9(004).
              10 BVVENN-E-AUT-CONTROLE                 PIC  9(002).
              10 BVVENN-E-FIM-OCORR                    PIC  X(001).

              10 BVVENN-E-OCORRENCIAS   OCCURS 25 TIMES.
                 15 BVVENN-E-AGENCIA                   PIC  9(005).
                 15 BVVENN-E-CONTA                     PIC  9(013).
                 15 BVVENN-E-BAND                      PIC  9(003).
                 15 BVVENN-E-VLR-BAND                  PIC  9(015)V9(2).

           05 BVVENN-AREA-SAIDA.
              10 BVVENN-S-COD-RETORNO                  PIC  9(002).
              10 BVVENN-S-MSG-RETORNO                  PIC  X(050).
              10 BVVENN-S-PROTOCOLO                    PIC  X(050).
              10 BVVENN-S-TMP-ESPERA                   PIC  X(008).
