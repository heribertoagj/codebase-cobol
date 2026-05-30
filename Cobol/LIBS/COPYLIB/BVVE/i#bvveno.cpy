      ******************************************************************
      *  NOVOS NORMATIVOS - RESOLUCAO 4734                             *
      *                                                                *
      ******************************************************************
      *                                                                *
      *  NOME DO BOOK..: I#BVVENO - AREA DO MODULO BVVEA515            *
      *  DESCRICAO.....: RETORNO DE GRAVAME DE RECEBIVEIS              *
      *  TAMANHO.......: 01141 BYTES                                   *
      *                                                                *
      *--ENTRADA-------------------------------------------------------*
      *                                                                *
      *  PROTOCOLO.....: PROTOCOLO PARA RASTREIO DE SOLICITACAO, GERA- *
      *                  DO PELO BVVEA514 NA INCLUSAO DO GRAVAME       *
      *                                                                *
      *--SAIDA---------------------------------------------------------*
      *                                                                *
      *  COD-RETORNO...: CODIGO DE RETORNO                             *
      *  MSG-RETORNO...: MENSGEM DE RETORNO                            *
      *  COD-SIT.......: ACEITO 'A' OU REJEITADO 'R'                   *
      *  MOT-REJ.......: MOTIVO REJEICAO CIP                           *
      *  ID-OPER.......: IDENTIFICADOR OPERACAO GERADO PELA CIP QUANDO *
      *                  A OPERACAO FOR ACEITA                         *
      *  BAND..........: BANDEIRA CORPORATIVA VISAO 2                  *
      *  VLR-GRAVAME...: VALOR GRAVAMADO PARA BANDEIRA                 *
      *                                                                *
      ******************************************************************
      *                                                                *
       01  BVVENO-COMMAREA.
           05 BVVENO-AREA-ENTRADA.
              10 BVVENO-E-PROTOCOLO                    PIC  X(050).
              10 BVVENO-R-QTDE-RESTART                 PIC  9(009).

           05 BVVENO-AREA-SAIDA.
              10 BVVENO-S-COD-RETORNO                  PIC  9(002).
              10 BVVENO-S-MSG-RETORNO                  PIC  X(050).
              10 BVVENO-S-COD-SIT                      PIC  X(001).
              10 BVVENO-S-ID-OPER-CIP                  PIC  X(019).
              10 BVVENO-S-OCORR   OCCURS 25.
                 15 BVVENO-S-AGENCIA                   PIC  9(005).
                 15 BVVENO-S-CONTA                     PIC  9(013).
                 15 BVVENO-S-BAND                      PIC  9(003).
                 15 BVVENO-S-VLR-GRAV-EFETV            PIC  9(015)V99.
              10 BVVENO-S-MOT-REJ                      PIC  X(060).

