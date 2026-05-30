      ******************************************************************
      * SISTEMA : DCOM - INTERFACE DEF VIA ISD - I#DCOMS3              *
      * TAMANHO : 1024 BYTES                                           *
      * ARQUIVO : SALDOS DIA ANTERIOR (INTERMEDIARIO)                  *
      ******************************************************************
       01  S3-REGISTRO.
      *    --->> CHAVE
  1 10     03  S3-DMOVTO-SLDO-DIA           PIC X(10).
 11  5     03  S3-CJUNC-DEPDC               PIC 9(05).
 16  5     03  S3-CPOSTO-SERVC              PIC 9(05).
 21  2     03  S3-CTPO-PAB                  PIC X(02).
 23  3     03  S3-SEGMENTO-CLIENTE          PIC 9(03).
 26  3     03  S3-CPRODT                    PIC 9(03).
 29  3     03  S3-CSPROD                    PIC 9(03).
 32  2     03  S3-CINDCD-CONTR-CONVE        PIC 9(02).
 34  9     03  S3-CCONTR-CONVE-DESC         PIC 9(09).
 43  3     03  S3-CVRSAO-CONTR-CONVE        PIC 9(03).
      *    --->> ATRIBUTOS
 46 17     03  S3-VLR-TTAL-A                PIC 9(15)V9(02).
 63 17     03  S3-VLR-TTAL-B                PIC 9(15)V9(02).
 80 17     03  S3-VLR-TTAL-C                PIC 9(15)V9(02).
 97 17     03  S3-VLR-TTAL-D                PIC 9(15)V9(02).
114 17     03  S3-VLR-TTAL-E                PIC 9(15)V9(02).
131 17     03  S3-VALOR-IOF                 PIC 9(15)V9(02).
148 17     03  S3-VALOR-TAC                 PIC 9(15)V9(02).
165 17     03  S3-VALOR-TARIF               PIC 9(15)V9(02).
182  5     03  S3-PRZO-OPER                 PIC 9(05).
187  5     03  S3-PRZO-TITL                 PIC 9(05).
192 15     03  S3-QTD-CONTRATO-DIA          PIC 9(15).
207 15     03  S3-QTD-TITULOS-DIA           PIC 9(15).
222 15     03  S3-QTD-CHEQUES-DIA           PIC 9(15).
237  9     03  S3-TX-ANO-OPER               PIC 9(03)V9(06).
246  9     03  S3-TX-MES-OPER               PIC 9(03)V9(06).
255 17     03  S3-SALDO-DIA                 PIC 9(15)V9(02).
      *    --->> SALDOS A VENCER
272 17     03  S3-SLDO-AVENC-14DIAS         PIC 9(15)V9(02).
289 17     03  S3-SLDO-AVENC-15A30DIAS      PIC 9(15)V9(02).
306 17     03  S3-SLDO-AVENC-31A59DIAS      PIC 9(15)V9(02).
323 17     03  S3-SLDO-AVENC-60A90DIAS      PIC 9(15)V9(02).
340 17     03  S3-SLDO-AVENC-ACIMADE90      PIC 9(15)V9(02).
      *    --->> SALDOS E QTDES CONTRATO/TITULO/CHEQUE VENCIDOS
357 17     03  S3-SLDO-VENCD-14DIAS         PIC 9(15)V9(02).
374 15     03  S3-QTDC-VENCD-15DIAS         PIC 9(15).
389 15     03  S3-QTDT-VENCD-15DIAS         PIC 9(15).
404 15     03  S3-QTDH-VENCD-15DIAS         PIC 9(15).
419 17     03  S3-SLDO-VENCD-15A30DIAS      PIC 9(15)V9(02).
436 15     03  S3-QTDC-VENCD-16A30DIAS      PIC 9(15).
451 15     03  S3-QTDT-VENCD-16A30DIAS      PIC 9(15).
466 15     03  S3-QTDH-VENCD-16A30DIAS      PIC 9(15).
481 17     03  S3-SLDO-VENCD-31A59DIAS      PIC 9(15)V9(02).
498 15     03  S3-QTDC-VENCD-31A59DIAS      PIC 9(15).
513 15     03  S3-QTDT-VENCD-31A59DIAS      PIC 9(15).
528 15     03  S3-QTDH-VENCD-31A59DIAS      PIC 9(15).
543 17     03  S3-SLDO-VENCD-60A90DIAS      PIC 9(15)V9(02).
560 15     03  S3-QTDC-VENCD-60A90DIAS      PIC 9(15).
575 15     03  S3-QTDT-VENCD-60A90DIAS      PIC 9(15).
590 15     03  S3-QTDH-VENCD-60A90DIAS      PIC 9(15).
605 17     03  S3-SLDO-VENCD-ACIMADE90      PIC 9(15)V9(02).
622 15     03  S3-QTDC-VENCD-ACIMADE90      PIC 9(15).
637 15     03  S3-QTDT-VENCD-ACIMADE90      PIC 9(15).
652 15     03  S3-QTDH-VENCD-ACIMADE90      PIC 9(15).
667 17     03  S3-SLDO-TOTAL                PIC 9(15)V9(02).
      *    --->> VALORES / QTDES E DATAS DIVERSOS
684 17     03  S3-VLR-CONTRATO-NO-VCTO      PIC 9(15)V9(02).
701 17     03  S3-VALOR-PRESENTE            PIC 9(15)V9(02).
718 17     03  S3-VALOR-MORA-LEGAL          PIC 9(15)V9(02).
735 17     03  S3-VLR-MORA-GERENCIAL        PIC 9(15)V9(02).
752 17     03  S3-VLR-IOF-VENCIDO           PIC 9(15)V9(02).
769 10     03  S3-DTTRANSF-CA               PIC X(10).
779 17     03  S3-VLR-TRANSF-CA             PIC 9(15)V9(02).
796 15     03  S3-QTDE-TTAL-CONTRATO        PIC 9(15).
811 15     03  S3-QTDE-TTAL-TITULOS         PIC 9(15).
826 15     03  S3-QTDE-TTAL-CHEQUES         PIC 9(15).
      *    --->> DADOS ADICIONAIS
841  4     03  S3-DANO-OPER-DESC            PIC 9(04).
845  9     03  S3-NSEQ-OPER-DESC            PIC 9(09).
854 15     03  S3-NPCELA-DESC               PIC 9(15).
869 17     03  S3-TTAL-VLR-FACE-OPER        PIC 9(15)V9(02).
886 17     03  S3-TTAL-VLR-FACE-PARC        PIC 9(15)V9(02).
903 13     03  S3-CCCORR                    PIC 9(13).
916109     03  FILLER                       PIC X(109).
