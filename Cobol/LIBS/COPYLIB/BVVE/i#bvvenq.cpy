      ******************************************************************
      *  NOVOS NORMATIVOS - RESOLUCAO 4734                             *
      *                                                                *
      ******************************************************************
      *                                                                *
      *  NOME DO BOOK..: I#BVVENQ - AREA DO MODULO BVVEA517            *
      *  DESCRICAO.....: RETORNA SITUACAO DE CANCELAMENTO DE GRAVAME   *
      *  TAMANHO.......: 00201 BYTES                                   *
      *                                                                *
      *--ENTRADA-------------------------------------------------------*
      *                                                                *
      *  PROTOCOLO.....: PROTOCOLO P/ RASTREIO DE SOLICITACAO GERADO   *
      *                  PELO BVVEA516, NO CANCELAMENTO DO GRAVAME     *
      *  CONTRATRO.....: NUMERO DO CONTRATO GRAN                       *
      *                                                                *
      *--SAIDA---------------------------------------------------------*
      *                                                                *
      *  COD-RETORNO...: CODIGO DE RETORNO                             *
      *  MSG-RETORNO...: MENSGEM DE RETORNO                            *
      *  ID-OPE-CANC...: GERADO PELA CIP NO CANCELAMENTO               *
      *  COD-SIT.......: CODIGO DA SITUACAO DA SOLICITACAO:            *
      *                   A-ACATADA / R-REJEITADA                      *
      *  MOT-REJ.......: MOTIVO DA REJEICAO DA SOLICITACAO             *
      *                                                                *
      ******************************************************************
      *                                                                *
       01  BVVENQ-COMMAREA.
           05 BVVENQ-AREA-ENTRADA.
              10 BVVENQ-E-PROTOCOLO                    PIC  X(050).
              10 BVVENQ-E-CONTRATO                     PIC  X(019).

           05 BVVENQ-AREA-SAIDA.
              10 BVVENQ-S-COD-RETORNO                  PIC  9(002).
              10 BVVENQ-S-MSG-RETORNO                  PIC  X(050).
              10 BVVENQ-S-ID-OPE-CANC                  PIC  X(019).
              10 BVVENQ-S-COD-SIT                      PIC  X(001).
              10 BVVENQ-S-MOT-REJ                      PIC  X(060).
