var selectedOverlay = null;
var weatherinfo = [];
var global_mountain = new Array();
var weather_array = new Array();
var global_lat = 0;
var global_dangerous = [];

function weather_func(lat, lon,mountain){
    var time =[];
    var moutain_obj = Object.assign({},mountain);



    timestamp = new Date().getTime();

    for ( var k=0; k<lat.length; k++ ) {
        var appid = "0b243535ca956ce7a1a437f965b4be63";
        //var appid = "c4247eb10da831c88686f8d58ba63ebe";
        let apiURI ="https://api.openweathermap.org/data/2.5/forecast?lat="+lat[k]+"&lon="+lon[k]+"&appid="+appid;
        let counter = 1;

        $.ajax({
            url: apiURI,
            dataType: "json",
            type: "GET",
            async: false,
            success: function (data) {
                var min = timestamp;
                var result;
                for (var i = 0; i < 40; i++) {

                    time[i] = timestamp - data.list[i].dt;

                    if (time[i] > 0 && min > time[i]) {
                        min = time[i];
                        result = i;
                    }
                }
                if ((data.list[result].rain) == undefined) {
                    data.list[result].rain = 0;

                } else if ((data.list[result].rain["1h"]) == undefined) {
                    if (data.list[result].rain["3h"] == undefined) {
                        data.list[result].rain = 0;
                    } else {
                        data.list[result].rain = data.list[result].rain["3h"];
                    }
                }
                if ((data.list[result].snow) == undefined) {
                    data.list[result].snow = 0;
                } else if ((data.list[result].snow["1h"]) == undefined) {
                    if (data.list[result].snow["3h"] == undefined) {
                        data.list[result].snow = 0;
                    } else {
                        data.list[result].snow = data.list[result].snow["3h"];

                    }
                }

                global_lat = lat;
                global_mountain.push(moutain_obj);
                var temp1=[];
                var temp = (data.list[result].main.temp) - 273.15;
                var humidity = data.list[result].main.humidity;
                var rain = data.list[result].rain;
                var speed = data.list[result].wind.speed;
                var snow = data.list[result].snow;
                var dew = (data.list[result].main.temp - 273.15) - (100 - data.list[result].main.humidity) / 5;
                var main = data.list[result].weather[0].main;
                var icon = data.list[result].weather[0].icon;

                var weather_1 = {temp, rain,  speed, humidity, dew, snow};
                var weather_2 ={temp, rain,  speed, humidity, dew, snow, main, icon};

                //weather_array[k] = weather_1;
                //weather_array.splice(k,0,weather_1);
                weather_array.push(weather_1);

                weatherinfo.push(weather_2);


                if(weather_array.length == mountain.length) {
                    $.ajaxSetup({async: false});
                    $.post("/weather", {
                        names: weather_array,
                        mountains: moutain_obj,
                        max_list: lat.length,
                    }, function (data) {
                        var dangerous = data;
                        // var x = '123';
                        dangerous = String(dangerous);
                        dangerous = dangerous.replace('[', '');
                        dangerous = dangerous.replace(']', '');
                        dangerous = dangerous.split(',');

                        // global_dangerous.push(data);
                        for (var i = 0 in dangerous) {
                            // dangerous = data.replace('[','');
                            dangerous[i] = Number(dangerous[i]);

                            global_dangerous.push(dangerous[i]);

                        }
                        // global_dangerous = {name :dangerous[0]};

                    });
                }
        }});

    };

}


var markers=[];
function showsido(first) {
    weatherinfo = [];
    weather_array = [];
    global_dangerous = [];
    global_mountain = [];
    var valname = first.options[first.selectedIndex].text;
    ps.keywordSearch(valname + ' 산', placesSearchCB,{
        category_group_code: "AT4",
        category_name: "여행 > 관광,명소 > 산"
    // size : '500'
    });
}

function showsigungu(sigun) {
    weatherinfo = [];
    weather_array = [];
    global_dangerous = [];
    global_mountain = [];
    var first = document.getElementById('selOne');
    var valname = first.options[first.selectedIndex].text;
    var sigun = sigun.options[sigun.selectedIndex].text;
    if (sigun == '---시,군,구---'){
        sigun = '';
    }
    ps.keywordSearch(valname + sigun + ' 산', placesSearchCB,{
        category_group_code: "AT4",
        category_name: "여행 > 관광,명소 > 산"
    // size : '500'
    });
}


var mapContainer = document.getElementById('map'), // 지도를 표시할 div
    mapOption = {
        center: new kakao.maps.LatLng(37.566826, 126.9786567), // 지도의 중심좌표
        level: 3 // 지도의 확대 레벨
    };

// 지도를 생성합니다
var map = new  kakao.maps.Map(mapContainer,mapOption);


//중심점으로 이동
function setCenter() {
    // 이동할 위도 경도 위치를 생성합니다
    var moveLatLon = new kakao.maps.LatLng(33.452613, 126.570888);

    // 지도 중심을 이동 시킵니다
    map.setCenter(moveLatLon);
}

//부드럽게 이동
function panTo(position_x,position_y) {
    // 이동할 위도 경도 위치를 생성합니다
    var moveLatLon = new kakao.maps.LatLng(position_x, position_y);

    // 지도 중심을 부드럽게 이동시킵니다
    // 만약 이동할 거리가 지도 화면보다 크면 부드러운 효과 없이 이동합니다
    map.panTo(moveLatLon);
}
// 장소 검색 객체를 생성합니다

var ps = new kakao.maps.services.Places();

// 키워드로 장소를 검색합니다
ps.keywordSearch('서울시 산', placesSearchCB,{
        category_group_code: "AT4",
        category_name: "여행 > 관광,명소 > 산"
});

// 키워드 검색 완료 시 호출되는 콜백함수 입니다
function placesSearchCB(data, status, pagination) {

    if (status === kakao.maps.services.Status.OK) {
        displayPlaces(data);

        // 페이지 번호를 표출합니다
        displayPagination(pagination);

    } else if (status === kakao.maps.services.Status.ZERO_RESULT) {

        alert('검색 결과가 존재하지 않습니다.');
        return;

    } else if (status === kakao.maps.services.Status.ERROR) {

        alert('검색 결과 중 오류가 발생했습니다.');
        return;

    }
}

function displayPlaces(places) {
    // weatherinfo=[];
    var listEl = document.getElementById('name_ul'),
    menuEl = document.getElementById('menu_wrap'),
    fragment = document.createDocumentFragment(),
    bounds = new kakao.maps.LatLngBounds(),
    listStr = '';
    // 검색 결과 목록에 추가된 항목들을 제거합니다
    removeAllChildNods(listEl);

    // 지도에 표시되고 있는 마커를 제거합니다
    removeMarker();
    var latList = new Array();
    var lonList = new Array();

    var mountain_name = new Array();
    var address = [];
    for ( var i =0 ; i<places.length; i++){
        latList[i] = places[i].y;
        lonList[i] = places[i].x;
        mountain_name[i] = places[i].place_name;
    }

    weather_func(latList, lonList, mountain_name);


    for ( var i=0; i<places.length; i++ ) {
            console.log(weatherinfo);

            lat = places[i].y;
            lon = places[i].x;

            // weather_func(lat,lon);
            latList[i] = places[i].y;
            lonList[i] = places[i].x;
            mountain_name[i] = places[i].place_name;



            var placePosition = new kakao.maps.LatLng(places[i].y, places[i].x),
            marker = addMarker(placePosition, i),
            itemEl = getListItem(i, places[i]); // 검색 결과 항목 Element를 생성합니다
            // 검색된 장소 위치를 기준으로 지도 범위를 재설정하기위해
            // LatLngBounds 객체에 좌표를 추가합니다
            bounds.extend(placePosition);


        // 마커와 검색결과 항목에 mouseover 했을때
        // 해당 장소에 인포윈도우에 장소명을 표시합니다
        // mouseout 했을 때는 인포윈도우를 닫습니다

        //익명 즉시실행함수
        //https://beomy.tistory.com/9 참고

        (function(marker, title, places,i) {

            //리스트에서 선택한 지점의 위도경도
            //itemEl에서 x,y가 정의되어 있지않다해서 다른 변수사용.
            var point_x = 0.0;
            var point_y = 0.0;
            point_x = places.x;
            point_y = places.y;

            kakao.maps.event.addListener(marker, 'onclick', function() {
                // nondisplayInfowindow();

                map.setLevel(3);
                overlay.setMap(map);
                panTo(point_y,point_x);

            });
            kakao.maps.event.addListener(marker, 'click', function() {
                displayOverlay(marker, title, places['address_name'],i);
            });

            itemEl.onclick = function(){

                map.setLevel(3);
                // overlay(map);
                panTo(point_y,point_x);
                displayOverlay(marker, title, places['address_name'],i);
                // overlay.setMap(map);
             }
             itemEl.onmouseover = function(){
                displayOverlay(marker, title, places['address_name'],i);
             }
             itemEl.onmouseout = function(){
                selectedOverlay.setMap(null)
             }


        })(marker,  places[i].place_name , places[i],i);

        fragment.appendChild(itemEl);
    }

    // 검색결과 항목들을 검색결과 목록 Elemnet에 추가합니다
    // weather_func(latList, lonList,mountain_name);

    listEl.appendChild(fragment);
    menuEl.scrollTop = 0;

    // 검색된 장소 위치를 기준으로 지도 범위를 재설정합니다
    map.setBounds(bounds);


}

function removeMarker() {
    for ( var i = 0; i < markers.length; i++ ) {
        markers[i].setMap(null);
    }
    markers = [];
}

// 검색결과 항목을 Element로 반환하는 함수입니다.
function getListItem(index, places) {

    var el = document.createElement('li'),
    itemStr = '<span class="markerbg marker_' + (index+1) + '"></span>' +
                '<div class="info">' +
                '<h5>' + places.place_name + '</h5>';

    if (places.road_address_name) {
        itemStr += '<span>' + places.road_address_name + '</span>' +
                    '<span class="jibun gray">' +  places.address_name  + '</span>';
    } else {
        itemStr += ' <span>' +  places.address_name  + '</span>';
    }
    el.innerHTML = itemStr;
    el.className = 'item';
    return el;
}

// 마커를 생성하고 지도 위에 마커를 표시하는 함수입니다
function addMarker(position, idx) {
    var url1=null;var url1=null;
    if(global_dangerous[idx] < 50){
        url1 =  '/static/green.png'
    }else if(global_dangerous[idx]>=50 &&global_dangerous[idx] < 70 ){
        url1 = '/static/yellow.png'
    }else if(global_dangerous[idx] >=70 && global_dangerous[idx] <90){
        url1= '/static/orange.png'
    }else{
        url1='/static/red.png'
    }
    var imageSrc = url1, // 마커 이미지 url, 스프라이트 이미지를 씁니다
    imageSize = new kakao.maps.Size(60, 60),  // 마커 이미지의 크기
    imgOptions =  {
        offset: new kakao.maps.Point(28, 26) // 마커 좌표에 일치시킬 이미지 내에서의 좌표
    },
    markerImage = new kakao.maps.MarkerImage(imageSrc, imageSize, imgOptions),
        marker = new kakao.maps.Marker({
        position: position, // 마커의 위치
        image: markerImage
    });


    marker.setMap(map); // 지도 위에 마커를 표출합니다
    markers.push(marker);  // 배열에 생성된 마커를 추가합니다

    return marker;
}

function displayOverlay(marker, place_name, sub_place=null,index) {
var content = '<style>\n' +
    '.wrap {position: absolute;border-radius:20px;left: 0;bottom: 30px;width: 350px;height: 250px;margin-left: -144px;text-align: left;overflow: hidden;font-size: 12px;font-family: \'Malgun Gothic\', dotum, \'돋움\', sans-serif;line-height: 1.5;}\n' +
    '.wrap * {padding: 0;margin: 0;}\n'+
    '.wrap .info_overlay {border-radius:10;width: 350px;height: 250px;border-radius: 5px;border-bottom: 2px solid #ccc;border-right: 1px solid #ccc;overflow: visible!important;background: #fff;}\n' +
    '.wrap .info_overlay:nth-child(1) {border: 0;box-shadow: 0px 1px 2px #888;}\n' +
    '.info_overlay .title {text-align:center;padding: 5px 0 0 10px;height: 30px;background: #323232;color: white;border-bottom: 1px solid #ddd;font-size: 16px;font-weight: bold;}\n' +
    '.info_overlay .close {position: absolute;top: 10px;right: 10px;color: #888;width: 17px;height: 17px;background: url("/static/overlay_close1.png");}\n'+
    '.info_overlay .close:hover {cursor: pointer;}\n'+
    '.info_overlay .body {position: relative;font-size: 1.2em;text-align: center;}\n'+
    '.info_overlay .desc {position: relative;margin: 30px 0 0 90px;height: 75px;}\n'+
    '.desc .ellipsis {overflow: hidden;text-overflow: ellipsis;white-space: nowrap; margin-top: 0.2em; font-weight: bolder;}\n'+
    '.desc .jibun {font-size: 11px;font-weight:100px;color: #888;margin-top: -2px;}\n'+
    '.info_overlay .img {margin-left: 2em;r;position: absolute;top: 6px;left: 5px;width: 120px;height: 117px;border: 1px solid #ddd;color: #888;overflow: hidden;}\n'+
    '.info_overlay:after {content: \'\';position: absolute;margin-left: -12px;left: 50%;bottom: 0;width: 22px;height: 12px;background: url("/static/vertex_white.png");}\n'+
    '.info_overlay .link {color: #5085BB;}\n' +
    '</style>' +
    '<div class="wrap">' +
            '    <div class="info_overlay">' +
            '        <div class="title">' + place_name +
            '            <div class="close" onclick="closeOverlay(selectedOverlay)" title="닫기"></div>' +
            '        </div>' +
            '        <div class="body">' +
            '            <div class="img">' +
                '               <img src="/static/weather/'+ weatherinfo[index].icon + '.png" width="120" height="117">' +
            '           </div>' +
            '            <div class="desc">' +
            // '                <div class="ellipsis" id="sub">'+"상세주소 :" + sub_place +'</div>' +
            '                <div class="ellipsis" id="sub">'+"온도 : " +weatherinfo[index].temp.toFixed(1) +"℃"+'</div>' +
                            '<div class="ellipsis" id="sub">'+"습도 : " + weatherinfo[index].humidity +"%"+'</div>' +
                            '<div class="ellipsis" id="sub">'+"강수량 : "+weatherinfo[index].rain +"mm"+'</div>' +
                            '<div class="ellipsis" id="sub">'+"적설량 : "+weatherinfo[index].snow +"mm"+'</div>' +
                            '<div class="ellipsis" id="sub">'+"날씨 : "+weatherinfo[index].main +'</div>' +
                            '<div class="ellipsis" id="sub">'+"위험도 : "+global_dangerous[index] +'</div>' +
    // '                        <div class="ellipsis" id="sub">'+sub_place +'</div>' +
            '            </div>' +
            '        </div>' +
            '    </div>' +
            '</div>';

    // 마커 위에 커스텀오버레이를 표시합니다
    // 마커를 중심으로 커스텀 오버레이를 표시하기위해 CSS를 이용해 위치를 설정했습니다
    var currentOverlay = new kakao.maps.CustomOverlay({
        content: content,
        map: map,
        position: marker.getPosition()
    });

    if (!selectedOverlay || selectedOverlay !== marker) {
        // 클릭된 마커 객체가 null이 아니면
        // 클릭된 마커의 이미지를 기본 이미지로 변경하고
        !!selectedOverlay && selectedOverlay.setMap(null);

        // 현재 클릭된 마커의 이미지는 클릭 이미지로 변경합니다
        currentOverlay.setMap(map);
    }
    // 커스텀 오버레이를 닫기 위해 호출되는 함수입니다
    selectedOverlay = currentOverlay;
}
function closeOverlay(overlay) {
    overlay.setMap(null);
}
function displayPagination(pagination) {
    var paginationEl = document.getElementById('pagination'),
        fragment = document.createDocumentFragment(),
        i;

    // 기존에 추가된 페이지번호를 삭제합니다
    while (paginationEl.hasChildNodes()) {
        paginationEl.removeChild (paginationEl.lastChild);
    }
    for (i=1; i<=pagination.last; i++) {
        var el = document.createElement('a');
        el.href = "#";
        el.innerHTML = i;

        if (i===pagination.current) {
            el.className = 'on';
        } else {
            el.onclick = (function(i) {
                return function() {

                    //placesSearchCB 재호출
                    weatherinfo = [];
                    weather_array = [];
                    global_dangerous = [];
                    global_mountain = [];
                    pagination.gotoPage(i);
                }
            })(i);
        }

        fragment.appendChild(el);
    }
    paginationEl.appendChild(fragment);
}


function removeAllChildNods(el) {

    while (el.hasChildNodes()) {
        el.removeChild (el.lastChild);
    }
}