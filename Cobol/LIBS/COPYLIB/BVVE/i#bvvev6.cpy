      *----------------------------------------------------------------*
      *  CONSTITUICAO DE GARANTIA DE ORPAG'S POR SALDO DEVEDOR         *
      *  INDENTIFICACAO DE RAV E VINCULO DE SALDO DEVEDOR              *
      *  I#BVVEV6 - ARQUIVO ENVIO/RETORNO DCOM PARA DESMONTE OPERACOES
      *  TAMANHO: 100                                                  *
      *----------------------------------------------------------------*

       01  BVVEV6-REGISTRO.
           03 BVVEV6-AGENCIA          PIC  9(005).
           03 BVVEV6-CONTA            PIC  9(013).
           03 BVVEV6-BAND             PIC  9(001).
           03 BVVEV6-CREDENC          PIC  X(020).
           03 BVVEV6-PROD-VISTA       PIC  9(005).
           03 BVVEV6-PROD-PARCEL      PIC  9(005).
           03 BVVEV6-VLR-DESMONTE     PIC  9(015)V9(002) COMP-3.
           03 BVVEV6-RETORNO-DCOM.
              05 BVVEV6-VLR-EFETIVADO PIC  9(015)V9(002) COMP-3.
           03 FILLER                  PIC  X(033).
