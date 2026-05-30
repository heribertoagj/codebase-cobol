      ******************************************************************
      * NOME BOOK : CTECWB0I                                           *
      * DESCRICAO : BOOK DE INTERFACE                                  *
      * DATA      : 02/2020                                            *
      * AUTOR     : MAURICIO VIEIRA JR                                 *
      * EMPRESA   : TCS CONSULTANCY SERVICES                           *
      * COMPONENTE: FUNCIONAL                                          *
      * TAMANHO   : 03124                                              *
      ******************************************************************
      * DATA       AUTOR             DESCRICAO / MANUTENCAO            *
      ******************************************************************
           05 CTECWB0I-HEADER.
              10 CTECWB0I-COD-LAYOUT         PIC X(08) VALUE 'CTECWB0I'.
              10 CTECWB0I-TAM-LAYOUT         PIC 9(05) VALUE 03124.
           05 CTECWJCI-REGISTRO.
              10 CTECWB0I-BLOCO-ENTRADA.
                 15 CTECWB0E-CMOTVO-ENCRR-CTA      PIC 9(03).
                 15 CTECWB0E-CSIT-MOTVO-ENCRR      PIC 9(01).
      ******************************************************************
              10 CTECWB0I-BLOCO-PAGINACAO.
                 15 CTECWB0I-INDICADOR-PAGINACAO
                                             PIC  X(001).
                    88 CTECWB0I-P-INICIAL    VALUE 'I'.
                    88 CTECWB0I-P-PRIMEIRA   VALUE 'P'.
                    88 CTECWB0I-P-SEGUINTE   VALUE 'S'.
                    88 CTECWB0I-P-ANTERIOR   VALUE 'A'.
                    88 CTECWB0I-P-ULTIMA     VALUE 'U'.
      ******************************************************************
                 15 CTECWB0I-PRI-CHAVE.
                    20 CTECWB0I-PRI-CMOTVO-ENCRR  PIC 9(03).
                    20 CTECWB0I-PRI-HINCL-REG     PIC X(26).
                 15 CTECWB0I-ULT-CHAVE.
                    20 CTECWB0I-ULT-CMOTVO-ENCRR  PIC 9(03).
                    20 CTECWB0I-ULT-HINCL-REG     PIC X(26).
      ******************************************************************
              10 CTECWB0I-BLOCO-SAIDA.
                 15 CTECWB0I-S-QTDE-CTA      PIC 9(04).
                 15 CTECWB0I-S-LISTA-CTA     OCCURS 0 TO 20 TIMES
                    DEPENDING ON CTECWB0I-S-QTDE-CTA.
                    20 CTECWB0I-S-CTA        PIC X(14).
                    20 CTECWB0I-S-NM         PIC X(40).
                    20 CTECWB0I-S-CPF        PIC X(19).
                    20 CTECWB0I-S-DENCR      PIC X(10).
                    20 CTECWB0I-S-SDO        PIC S9(015)V99.
                    20 CTECWB0I-S-INICIATV   PIC X(10).
                    20 CTECWB0I-S-MOTIV      PIC X(40).
