      *---------------------------------------------------------------*
      *               C A P G E M I N I                               *
      *---------------------------------------------------------------*
      * INCLUDE.....: I#DCOMXS - PARA MODULO RUEC8850                 *
      * GERACAO.....: JULHO/2020                                      *
      *---------------------------------------------------------------*
      * OBJETIVO....: INCLUSAO DOS DADOS RETORNADOS PELA COR0007R1    *
      *               NA TABELA RUECB046                              *
      *---------------------------------------------------------------*
      * PROJETO ....: REFORMULACAO MENSAGERIA BACEN - RECOR           *
      *---------------------------------------------------------------*
      **
       01  DCOMXS-AREA.
           05  DCOMXS-ENVIO.
             10  DCOMXS-PGM-CHAMADOR         PIC  X(008).
             10  DCOMXS-NRO-CTRLIF           PIC  X(020).
             10  DCOMXS-NUM-REF-BCCOR        PIC  X(011).
             10  DCOMXS-ANO-MES-REF          PIC  X(007).
             10  DCOMXS-SIT-OPCOR            PIC  X(034).
             10  DCOMXS-DANO-OPER-DESC       PIC  9(004).
             10  DCOMXS-NSEQ-OPER-DESC       PIC  9(009).


      **
      *************** AREA DE DADOS DE RETORNO ***************
      **
           05  DCOMXS-RETORNO.
             10  DCOMXS-COD-RET              PIC  9(005).
      **
      **=======  00 - OK
      **=======  01 - DADOS INCONSISTENTES
      **=======  03 - NAO ENCONTRADO
      **=======  99 - ERRO DB2
             10  DCOMXS-MENSAGEM             PIC  X(079).
             10  FILLER                      PIC  X(050).
             10  DCOMXS-SQLCA                PIC  X(136).
             10  DCOMXS-ERRO-AREA            PIC  X(107).
             10  DCOMXS-NRO-CTRL-IF-S        PIC  X(020).
             10  DCOMXS-NUM-REF-BCCOR-S      PIC  X(011).
             10  DCOMXS-ANO-MES-REF-S        PIC  X(007).
             10  DCOMXS-SIT-OPCOR-S          PIC  X(034).
             10  DCOMXS-TP-BENEF             PIC  X(001).
             10  DCOMXS-CNPJ-CPF-BENEF       PIC  X(019).
             10  DCOMXS-CNPJ-RESPON          PIC  X(019).
             10  DCOMXS-DT-INI-PESQ          PIC  X(010).
             10  DCOMXS-DT-FIM-PESQ          PIC  X(010).
             10  DCOMXS-NUM-ORDEM-DESTC      PIC  X(003).
             10  DCOMXS-VLR-PARCL-CRED       PIC  X(019).
             10  DCOMXS-IDENTF-SCR           PIC  X(044).
             10  DCOMXS-VLR-SLDMEDDIARIO     PIC  X(019).
             10  DCOMXS-VLR-SLDULTDIA        PIC  X(019).
             10  DCOMXS-VLR-SLDPRICULTDIA    PIC  X(019).
             10  DCOMXS-VLR-SLDVINCNDULTDIA  PIC  X(019).
             10  DCOMXS-VLR-SLDPRIVINULTDIA  PIC  X(019).
             10  DCOMXS-VLR-SLDMEDDIAVIN     PIC  X(019).
             10  DCOMXS-TP-BONUSREBT         PIC  X(002).
             10  DCOMXS-DT-PGTO-BONUS        PIC  X(010).
             10  DCOMXS-VLR-BONUSREBT        PIC  X(019).
             10  DCOMXS-DTHR-BC              PIC  X(018).
             10  DCOMXS-DATA-MOVTO           PIC  X(010).
